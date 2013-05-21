{-# LANGUAGE ScopedTypeVariables #-}

module Crawler where

import Common
import PageDownloader
import PageProcessor

import qualified Data.Set as S
import Data.Char(toLower)

import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Control.Concurrent
import Control.Exception(finally)
import System.IO.Unsafe

mkPool :: Int -> Int -> IO (l -> IO (Fallible r) -> IO (), IO [(l,Fallible r)])
mkPool size max_tasks = do
  tasks <- newChan
  result <- newEmptyMVar
  numTasks <- newMVar 0
  let worker = do
--         putStrLn "waiting for task"
         (label, act) <- readChan tasks
--         putStrLn $ "took " ++ show label
         res <- act
         putMVar result (label, res)
         worker
  worker_tids <- sequence . replicate size . forkIO $ worker
  let loop remaining = do
         num_tasks <- readMVar numTasks
         case () of
           _ | remaining == 0 -> warn "task quota exceeded" >> return []
             | num_tasks == 0 -> info "grabbing done" >> return []  -- закончили обработку, закончились задания
             | otherwise -> do r <- takeMVar result
                               let remaining' = case r of -- в ограничении глубины учитываем только разобранные страницы
                                     (_, Left _) -> remaining
                                     (_, Right _) -> remaining - 1
                               modifyMVar_ numTasks (\c -> return (c-1))
                               rest <- loop remaining'
                               return $ r:rest
      putTask label act = do
        modifyMVar_ numTasks (\c -> return $ c+1)
        writeChan tasks (label, act)
  return (putTask, loop max_tasks `finally` mapM_ killThread worker_tids)

type SeenStorage = MVar (S.Set URI)
type TaskAdder a = URI -> IO a -> IO ()

-- Обёртка над пулом потоков, организующая обход сайта, ведущая учёт посещённых страниц
crawleSite :: Int -> URI -> IO [(URI, Fallible Tags)]
crawleSite quota start_page = do
  (add_task, run_pool) <- mkPool numWorkers quota
  seen :: MVar (S.Set URI) <- newMVar S.empty

  add_task start_page (crawler seen add_task start_page)
  run_pool

-- Функция, анализирующая конкретный URL, выставляющая задачи на анализ обнаруженных ссылок
crawler :: SeenStorage -> TaskAdder (Fallible Tags) -> URI -> IO (Fallible Tags)
crawler seen add_task url = do
  content_type <- getContentType url -- на первом этапе проверяем content type
  case content_type of
    Left err -> return $ Left err
    Right ctype | map toLower ctype == "text/html" -> crawlePage
    Right ctype -> return $ Left $ "skipping " ++ show url ++ " having content type " ++ ctype
 where
   crawlePage = do
     res <- getPage url
     case res of
       Right page -> do
         let tag_stream = parseHtml $ page
             childs = S.fromList . getLinks url $ tag_stream
         new_childs <- modifyMVar seen $ \seen_set -> do
           let new = S.difference childs seen_set
           return $ (S.union seen_set new, S.toList new)
         mapM_ (\link -> add_task link (crawler seen add_task link)) new_childs
         return $ Right tag_stream
       Left err -> return $ Left err
