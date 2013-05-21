-- Модуль индексации страницы
module PageIndex where

import Common
import PageProcessor

import Network.URI

import qualified Data.Map as M
import Data.List(foldl')

type PageIndex = M.Map Word Int
type WordStats = M.Map URI Int
type GlobalIndex = M.Map Word WordStats

-- индексируем содержимое одной страницы
indexContent :: [Word] -> PageIndex
indexContent content = let m = foldl' (\dic w -> M.insertWith (+) w 1 dic) M.empty content
                       in M.foldl' (+) 0 m `seq` m

-- строим глобальный индекс по индексу страниц
indexPages :: [(URI, PageIndex)] -> GlobalIndex
indexPages page_indexes = M.unionsWith combine_word_stats $ map page_to_global page_indexes
  where page_to_global (url,page_index) = M.map (\i -> M.singleton url i) page_index
        combine_word_stats :: WordStats -> WordStats -> WordStats
        combine_word_stats = M.unionWith (+)

printGlobalIndex :: GlobalIndex -> IO ()
printGlobalIndex gi = mapM_ (\(word, page_index) -> putStrLn word >> printPageIndex page_index) $ M.toList gi

printPageIndex :: WordStats -> IO ()
printPageIndex pi = mapM_ (\(uri, num) -> putStrLn $ "   " ++ show uri ++ show num) $ M.toList pi

