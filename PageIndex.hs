-- Модуль индексации страницы и построения глобального индекса
module PageIndex(GlobalData(..), PageData(..),
                 indexContent, addPage, printDatabase,
                 emptyDatabase) where

import Common
import PageProcessor

import Network.URI

import qualified Data.Map.Strict as M
import Data.List(foldl')
import Text.Printf
import Control.Applicative

import qualified Data.Text.IO as T

type PageIndex = M.Map Word Float -- статистика частот по одной странице
data PageData = PageData {pdIndex :: !PageIndex, pdTitle :: !Title} -- собираемые данные по странице

type WordStats = M.Map URI Float
type GlobalIndex = M.Map Word WordStats
type TitleIndex = M.Map URI Word

data GlobalData = GlobalData {gdIndex :: !GlobalIndex, gdTitles :: !TitleIndex}

-- вспомогательная функция, для внесения строгости
traverse m = M.foldl' (+) 0 m

-- индексируем содержимое одной страницы
indexContent :: [Word] -> PageIndex
indexContent content = let m = foldl' (\dic word -> M.insertWith (+) word weight dic) M.empty content
                           num_words = length content
                           weight = 1 / fromIntegral num_words
                       in traverse m `seq` m

-- пустой индекс
emptyDatabase = GlobalData M.empty M.empty

-- добавление очередной страницы к индексу
addPageToIndex :: GlobalIndex -> (URI, PageIndex) -> GlobalIndex
addPageToIndex global_index (url, page_index) = M.unionWith combine_word_stats global_index local_piece
  where local_piece =  M.map (\i -> M.singleton url i) page_index
        combine_word_stats :: WordStats -> WordStats -> WordStats
        combine_word_stats = M.unionWith (+)

addPage :: GlobalData -> (URI, PageData) -> GlobalData
addPage (GlobalData index titles) page@(url,page_data) = GlobalData (addPageToIndex index (pdIndex <$> page)) (M.insert url (pdTitle page_data) titles)

-- печатаем глобальный индекс
printDatabase :: GlobalData -> IO ()
printDatabase gd = mapM_ (\(word, page_index) -> T.putStrLn word >> printPageIndex page_index) $ M.toList $ gdIndex gd

-- печатаем информацию по слову (часть индекса)
printPageIndex :: WordStats -> IO ()
printPageIndex pi = mapM_ (\(uri, freq) -> putStrLn $ printf "   %50s %10f" (show uri) freq) $ M.toList pi
