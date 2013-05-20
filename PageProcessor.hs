-- Модуль, обрабатывающий Html страницу
module PageProcessor where

import Network.URI

import Data.List(find)
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
          Nothing -> site {uriPath = drop_fragment u, uriQuery = ""} -- исходим из того, что это относительная урла. Подмена не совсем корректная, но вреда не будет
          Just abs -> abs
        drop_fragment = fst . break (=='#')

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