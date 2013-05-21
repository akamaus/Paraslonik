-- Модуль, обрабатывающий Html страницу
module PageProcessor where

import Network.URI

import Common

import Data.List(find, intercalate)
import Data.Char(isAlpha)
import Text.HTML.TagSoup
import Data.Maybe(fromMaybe)

type Tags = [Tag String]

-- Разбирает Html в поток тегов
parseHtml = dropTags ["script", "style"] . parseTags

-- Читает ссылки из потока тегов, приводит их к одному виду, выбрасывает внешние
getLinks :: URI -> Tags -> [URI]
getLinks site tags = procLinks site $ map (fromAttrib "href") . filter (~== "<a href>") $ tags

procLinks site links = filter internal . map canonicalize . filter (\l -> non_fragment l && non_query l) $ links
  where internal u = uriAuthority u == uriAuthority site
        canonicalize u = fromMaybe (site {uriPath = dots_processor . make_absolute (uriPath site) $ u, uriQuery = ""}) -- исходим из того, что это относительная урла. Подмена не совсем корректная, но вреда не будет
                                   (parseURI u)
        non_fragment = notElem '#'
        non_query = notElem '?'

make_absolute old cur = case cur of
  ('/':_) -> cur -- абсолютный путь
  _ -> case take 1 (reverse old) of
    [] -> "/" ++ cur
    "/" -> old ++ cur -- сейчас мы в каталоге
    _ -> old ++ "/../" ++ cur -- мы находимся на страничке, переходим к другому документу в том же каталоге

dots_processor path@('/':_) = intercalate "/" $ proc_dots [] $ split (=='/') path
dots_processor [] = []
dots_processor p = error $ "processing dots only in absolute paths, but called with " ++ p

split _ [] = []
split test lst = let (cur, next) = break test lst
                 in case next of
                   "/" -> cur : [""]
                   _ -> cur : split test (drop 1 next)

proc_dots acc (".":xs) = proc_dots acc xs
proc_dots acc ("..":xs) = proc_dots (drop 1 acc) xs
proc_dots acc (x:xs) = proc_dots (x:acc) xs
proc_dots acc [] = reverse acc

-- Читаем слова из потока тегов, чистим их от мусора
getWords :: Tags -> [Word]
getWords = map cleanWord . words . innerText

cleanWord = reverse . dropWhile (not . isAlpha) . reverse . dropWhile (not . isAlpha)

-- Вырезаем содежимое заданных тегов из документа
dropTags :: (Eq str) => [str] -> [Tag str] -> [Tag str]
dropTags labels [] = []
dropTags labels (tag:rest) =
  case find (flip isTagOpenName tag) labels of
    Nothing -> tag : dropTags labels rest
    Just target -> skipTo target rest
  where
    skipTo target [] = []
    skipTo target (tag:rest)
      | isTagCloseName target tag = dropTags labels rest
      | otherwise = skipTo target rest