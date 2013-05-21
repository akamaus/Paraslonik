-- Модуль индексации страницы
module PageIndex where

import Common
import PageProcessor

import Network.URI

import qualified Data.Map as M
import Data.List(foldl')

type PageIndex = M.Map Word Float
type WordStats = M.Map URI Float
type GlobalIndex = M.Map Word WordStats

-- вспомогательная функция, для внесения строгости
traverse m = M.foldl' (+) 0 m

-- индексируем содержимое одной страницы
indexContent :: [Word] -> PageIndex
indexContent content = let m = foldl' (\dic word -> M.insertWith (+) word weight dic) M.empty content
                           num_words = length content
                           weight = 1 / fromIntegral num_words
                       in traverse m `seq` m

-- пустой индекс
emptyGlobalIndex = M.empty :: GlobalIndex

-- добавление очередной страницы к индексу
addPageToIndex :: GlobalIndex -> (URI, PageIndex) -> GlobalIndex
addPageToIndex global_index (url, page_index) = M.unionWith combine_word_stats global_index local_piece
  where local_piece =  M.map (\i -> M.singleton url i) page_index
        combine_word_stats :: WordStats -> WordStats -> WordStats
        combine_word_stats = M.unionWith (+)

-- строим глобальный индекс по индексу страниц
indexPages :: [(URI, PageIndex)] -> GlobalIndex
indexPages page_indexes = M.unionsWith combine_word_stats $ map page_to_global page_indexes
  where page_to_global (url,page_index) = M.map (\i -> M.singleton url i) page_index
        combine_word_stats :: WordStats -> WordStats -> WordStats
        combine_word_stats = M.unionWith (+)

-- печатаем глобальный индекс
printGlobalIndex :: GlobalIndex -> IO ()
printGlobalIndex gi = mapM_ (\(word, page_index) -> putStrLn word >> printPageIndex page_index) $ M.toList gi

-- печатаем информацию по слову (часть индекса)
printPageIndex :: WordStats -> IO ()
printPageIndex pi = mapM_ (\(uri, num) -> putStrLn $ "   " ++ show uri ++ show num) $ M.toList pi

