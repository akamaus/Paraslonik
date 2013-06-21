{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings #-}

module Crawler(IndexRestrictions(..), mkSiteCrawler) where

import Common
import PageDownloader
import PageProcessor

import qualified Data.Set as S

import qualified Data.Text as T

import Control.Concurrent.Chan

import Control.Concurrent
import Control.Exception(SomeException,finally,handle)

import Text.Regex.TDFA
import Text.Regex.Base

import Network.URI

-- создаёт thread pool
-- аргументы: размер, максимальное количество заданий (предел по глубине)
-- результат: функция добавления новой задачи, функция запускающая работу и подсчитывающая итог
-- расшифровка полиморфных аргументов:
-- l - метка результата
-- t - итог,
-- r - результат,
-- p - трансформированный результат
mkPool :: Int -> Maybe Int -> IO (l -> IO (Fallible r) -> IO (), t -> ((l,r)->p) -> (t -> p -> t) -> IO t)
mkPool size max_tasks = do
  tasks <- newChan
  result <- newEmptyMVar
  numTasks <- newMVar 0
  let worker = do
         (label, act) <- readChan tasks
         handle (\(e :: SomeException) -> putMVar result (label, Left $ "Got exception: " ++ show e)) $ do
           res <- act
           putMVar result (label, res) -- рабочий поток ждет, пока его результаты не понадобятся
         worker
  worker_tids <- sequence . replicate size . forkIO $ worker -- запускаем рабочие лошадки
  let loop !remaining !total processor summer = do -- функция, перерабатывающая полученные результаты
         num_tasks <- readMVar numTasks
         case () of
           _ | remaining == Just 0 -> warn "task quota exceeded" >> return total -- прерываем обработку, превышено количество заданий
             | num_tasks == 0 -> info "grabbing done" >> return total  -- закончили обработку, закончились задания
             | otherwise -> do r <- takeMVar result
                               modifyMVar_ numTasks (\c -> return (c-1))
                               (remaining', total') <- case r of -- в ограничении глубины учитываем только разобранные страницы
                                     (_, Left err) -> warn err >> return (remaining, total)
                                     (u, Right res) -> return (pred <$> remaining, total `summer` processor (u,res))
                               loop remaining' total' processor summer
      putTask label act = do -- функция, добавляющая новое задание
        modifyMVar_ numTasks (\c -> return $ c+1)
        writeChan tasks (label, act)
  return (putTask, \total0 processor summer -> loop max_tasks total0 processor summer `finally` mapM_ killThread worker_tids)

type SeenStorage = MVar (S.Set URI) -- тип, хранилище посещенных ссылок
type TaskAdder m a = URI -> m a -> m ()

-- Функция, анализирующая конкретный URL, выставляющая задачи на анализ обнаруженных ссылок
crawler :: Depth -> SeenStorage -> TaskAdder IO (Fallible Tags) -> URI -> Downloader Tags
crawler depth seen add_task url = do
  ctype <- getContentType url -- на первом этапе проверяем content type
  case () of
    _ | T.toLower ctype == "text/html" -> crawlePage -- далее обходим страницу
      | otherwise -> fail $ "skipping " ++ show url ++ " having content type " ++ T.unpack ctype
 where
   crawlePage = do
     de <- ask
     let restrictions = deRestrictions de
     let domainFilter link = testRestriction (irDomain restrictions) $ \dom ->
           T.isSuffixOf (T.pack dom) (T.pack . uriRegName . fromMaybe (error $ "relative link" ++ show link) . uriAuthority $ link)
         queryFilter link = testRestriction (irQueryTest restrictions) $ \query ->
           match query (uriQuery link)
         ignoreQueryFilter link = testRestriction (irIgnoreQueryTest restrictions) $ \iquery ->
           not $ match iquery (uriQuery link)

     page <- getPage url
     let tag_stream = parseHtml $ page
         links = getLinks url $ tag_stream
         filtered_links = filter ignoreQueryFilter . filter queryFilter . filter domainFilter $ links
         childs = S.fromList $ filtered_links
     new_childs <- liftIO $ modifyMVar seen $ \seen_set -> do -- фильтруем новые ссылки, обновляем множество посещенных страниц
       let new = S.difference childs seen_set
       return $ (S.union seen_set new, S.toList new)

     when (testRestriction (irDepth restrictions) (depth <)) $ liftIO $ do
       mapM_ (\link -> add_task link (runDownloader (crawler (depth + 1) seen add_task link) de)) new_childs -- все ссылки помещаем в очередь на анализ
     return tag_stream -- возвращаем поток тегов для дальнейшей обработки

testRestriction mr test = case mr of
  Nothing -> True
  Just r -> test r

-- Обёртка над пулом потоков, организующая обход сайта, ведущая учёт посещённых страниц
mkSiteCrawler :: IndexRestrictions -> URI -> IO (t -> ((URI, Tags) -> p) -> (t -> p -> t) -> IO t, MVar Statistics)
mkSiteCrawler restrictions start_page = do
  (add_task, run_pool) <- mkPool (irNumWorkers restrictions) (irMaxPages restrictions) -- создали пул
  seen :: SeenStorage <- newMVar S.empty -- множество посещенных страниц, чтоб избежать петлей ссылок
  stats_var <- newMVar emptyStats
  let env0 = DownloaderEnv { deRestrictions = restrictions, deStats = stats_var }
  add_task start_page (runDownloader (crawler 1 seen add_task start_page) env0) -- добавили первую страницу
  return $ (run_pool, stats_var) -- вернули обработчик, его запустят выше
