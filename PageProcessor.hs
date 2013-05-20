-- Модуль, обрабатывающий Html страницу
module PageProcessor where

import Network.URI

import Data.List(find, intercalate)
import Data.Char(isAlpha)
import Text.HTML.TagSoup

type Tags = [Tag String]

-- Разбирает Html в поток тегов
parseHtml = dropTags ["script", "style"] . parseTags

-- Читает ссылки из потока тегов, приводит их к одному виду, выбрасывает внешние
getLinks :: URI -> Tags -> [URI]
getLinks site tags = filter internal . map (canonicalize . fromAttrib "href") . filter (~== "<a href>") $ tags
  where internal u = uriAuthority u == uriAuthority site
        canonicalize u = case parseURI u of
          Nothing -> site {uriPath = dots_processor . make_absolute (uriPath site) . drop_fragment $ u, uriQuery = ""} -- исходим из того, что это относительная урла. Подмена не совсем корректная, но вреда не будет
          Just abs -> abs
        drop_fragment = fst . break (=='#')
        make_absolute old cur = case cur of
          ('/':_) -> cur -- абсолютный путь
          _ -> case take 1 (reverse old) == "/" of
            False -> old ++ "/" ++ cur -- относительный путь
            True -> old ++ cur -- относительный путь

dots_processor path@('/':_) = intercalate "/" $ proc_dots [] $ split (=='/') path
dots_processor [] = []
dots_processor _ = error "processing dots only in absolute paths"

split _ [] = []
split test lst = let (cur, next) = break test lst
                 in cur : split test (drop 1 next)

proc_dots acc (".":xs) = proc_dots acc xs
proc_dots acc ("..":xs) = proc_dots (drop 1 acc) xs
proc_dots acc (x:xs) = proc_dots (x:acc) xs
proc_dots acc [] = reverse acc

-- Читаем слова из потока тегов, чистим их от мусора
getWords = filter (not . null) . map clean_word . words . innerText
  where clean_word = reverse . dropWhile (not . isAlpha) . reverse . dropWhile (not . isAlpha)

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