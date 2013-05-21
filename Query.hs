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
data Result = Exact URI Rating | Partial URI Word Float

instance Show Result where
  show (Exact u r) = printf "%-50s %f" (show u) r
  show (Partial u w r) = printf "%-50s %-10s   %f" (show u) w r

-- отыскивает наиболее подходящие страницы
findPages :: GlobalIndex -> [String] -> [Result]
findPages index [] = []
findPages index query = let
  relevant_pages = map (\w -> fromMaybe M.empty $ M.lookup w index) query
  top = foldl1 (M.intersectionWith (*)) relevant_pages
  in case () of
    _ | M.size top > 0 -> let top_urls = sortBy (compare `on` (negate . snd)) $ M.toList top -- точное совпадение по всем словам запроса
                          in map (uncurry Exact) top_urls
      | otherwise -> let word_data = filter ((> 0) . M.size . snd) $ zip query relevant_pages
                     in map (\(word, word_stats) -> let (url, stats) = maximumBy (compare `on` snd) $ M.toList $ word_stats
                                                    in Partial url word stats
                            ) word_data
