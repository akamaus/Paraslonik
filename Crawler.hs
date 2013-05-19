module Crawler where

import PageDownloader
import PageProcessor

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

crawleSite :: URI -> IO ()
crawleSite start_page = do
  (add_task, run_pool) <- mkPool num_workers
  let crawler p = do
        res <- getPage p
        case res of
          Right page -> do
            print $ "visited " ++ show p
            let childs = getLinks p $ parseHtml page
            mapM_ (\c -> add_task c (crawler c)) childs
          Left err -> do
            print $ "got error" ++ show err
  add_task start_page (crawler start_page)
  run_pool >>= mapM_ print
  