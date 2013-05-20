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

indexContent :: [Word] -> PageIndex
indexContent = foldl' (\dic w -> M.insertWith (+) w 1 dic) M.empty

indexPages :: [(URI, PageIndex)] -> GlobalIndex
indexPages page_indexes = M.unionsWith combine_word_stats $ map page_to_global page_indexes
  where page_to_global (url,page_index) = M.map (\i -> M.singleton url i) page_index
        combine_word_stats :: WordStats -> WordStats -> WordStats
        combine_word_stats = M.unionWith (+)
