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
import System.IO.Unsafe

num_workers = 1

mkPool :: Show l => Int -> IO (l -> IO r -> IO (), IO [(l,r)])
mkPool size = do
  tasks <- newChan
  results <- newChan
  numTasks <- newMVar 0
  let worker = do
--         putStrLn "waiting for task"
         (label, act) <- readChan tasks
--         putStrLn $ "took " ++ show label
         res <- act
         writeChan results (label, res)
         worker
  worker_tids <- sequence . replicate size . forkIO $ worker
  let loop = do
         num_tasks <- readMVar numTasks
--         putStrLn $ "num tasks " ++ show num_tasks
         case () of
           _ | num_tasks == 0 -> do mapM_ killThread worker_tids -- закончили обработку
                                    return []
             | otherwise -> do r <- readChan results
                               modifyMVar_ numTasks (\c -> return (c-1))
                               rest <- loop -- unsafeInterleaveIO loop
                               return $ r:rest
      putTask label act = do
        modifyMVar_ numTasks (\c -> return $ c+1)
        writeChan tasks (label, act)
  return (putTask, loop)

type SeenStorage = MVar (S.Set URI)
type TaskAdder a = URI -> IO a -> IO ()
type Processor a = Fallible Tags -> Fallible a

crawleSite :: URI -> IO ()
crawleSite start_page = do
  (add_task, run_pool) <- mkPool num_workers
  seen :: MVar (S.Set URI) <- newMVar S.empty

  add_task start_page (crawler seen add_task processor start_page)
  run_pool >>= mapM_ print

processor (Left err) = Left err
processor (Right tags) = Right $ length tags

crawler :: SeenStorage -> TaskAdder (Fallible a) -> Processor a -> URI -> IO (Either String a)
crawler seen add_task processor url = do
  res <- getPage url
  info $ "visited " ++ show url
  case res of
    Right page -> do
      let tag_stream = parseHtml $ page
      let childs = S.fromList . getLinks url $ tag_stream
      new_childs <- modifyMVar seen $ \seen_set -> do
        let new = S.difference childs seen_set
        return $ (S.union seen_set new, S.toList new)
      mapM_ (\c -> do
                ectype <- getContentType c
                case ectype of
                  Left err -> putStrLn $ "error probing " ++ show c ++ " : " ++ err
                  Right ctype | map toLower ctype == "text/html" -> do debug $ "putting new page in queue " ++ show c
                                                                       add_task c (crawler seen add_task processor c)
                  Right ct -> info $ "skipping " ++ show c ++ " having content type " ++ ct) new_childs
      return . processor $ Right tag_stream
    Left err -> do
      warn $ "got error" ++ show err
      return . processor $ Left err
