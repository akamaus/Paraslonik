-- выполнение поисковых запросов по базе
module Query where

import Common
import PageIndex

import Network.URI

import Data.Maybe
import Data.Function

import qualified Data.Map as M
import Data.List(sortBy, maximumBy)
import Text.Printf

type Rating = Float

-- результат
data Result = Exact URI Title Rating -- полное совпадение, по всем словам
            | Partial URI Title Word Float -- частичное, по одному слову

-- инстанс, для более аккуратного вывода
instance Show Result where
  show (Exact u t r) = printf "%-70s %-50s %f" (show u) t r
  show (Partial u t w r) = printf "%-70s %-50s %-10s   %f" (show u) t w r

-- отыскивает наиболее подходящие страницы
findPages :: GlobalData -> [Word] -> [Result]
findPages _ [] = []
findPages database query = let
  relevant_pages = map (\w -> fromMaybe M.empty $ M.lookup w $ gdIndex database) query -- все страницы, где есть слова запроса
  top = foldl1 (M.intersectionWith (*)) relevant_pages -- пересекаем множества страниц, получаем полные совпадения
  in case () of
    _ | M.size top > 0 -> let top_urls = sortBy (compare `on` (negate . snd)) $ M.toList top -- точное совпадение по всем словам запроса
                          in map (\(u,r) -> Exact u (lookupTitle u) r) top_urls
      | otherwise -> let word_data = filter ((> 0) . M.size . snd) $ zip query relevant_pages -- нет точных совпадений, ищем частичные
                     in map (\(word, word_stats) -> let (url, stats) = maximumBy (compare `on` snd) $ M.toList $ word_stats
                                                    in Partial url (lookupTitle url) word stats
                            ) word_data
 where lookupTitle url = fromMaybe "<Unknown>" $ M.lookup url $ gdTitles database
