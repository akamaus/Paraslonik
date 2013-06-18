{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
-- Модуль, обрабатывающий Html страницу
module PageProcessor where

import Common

import Network.URI

import qualified Data.Text as T
import Data.Char(isAlpha)

import Data.List(find, intercalate)
import Text.HTML.TagSoup
import Data.Maybe(fromMaybe, catMaybes)
import Control.Applicative
import Data.Function(on)

type Tags = [Tag Document]

instance TagRep T.Text where
    toTagRep x = case parseTags x of
                     [a] -> toTagRep a
                     _ -> error $ "When using a TagRep it must be exactly one tag, you gave: " ++ show x


-- Разбирает Html в поток тегов
parseHtml = dropTags ["script", "style"] . parseTags

-- Читает ссылки из потока тегов, приводит их к одному виду
getLinks :: URI -> Tags -> [URI]
getLinks site tags = procLinks site $ map (T.unpack . fromAttrib "href") . filter (~== T.pack "<a href>") $ tags

procLinks :: URI -> [String] -> [URI]
procLinks site =
  map normalizeUrl . filter httpUris . map (relativeTo `flip` site) .
  catMaybes . map (parseURIReference . (\u -> if all isAllowedInURI u then u else escapeURIString isUnescapedInURI u))
 where httpUris uri = elem (uriScheme uri) ["http:", "https:"]

normalizeUrl u = hidePort $ u {uriFragment = ""}
  where hidePort u | uriScheme u == "http:" = case uriAuthority u of
          Just auth | uriPort auth == ":80" -> u {uriAuthority = Just auth {uriPort = ""}}
          _ -> u
        hidePort u = u

-- получаем заголовок страницы
getTitle :: Tags -> Title
getTitle tags = cleanWord . innerText . takeWhile (not . isTagCloseName  "title") . dropWhile (not . isTagOpenName "title") $ tags

-- Читаем слова из потока тегов, чистим их от мусора
getWords :: Tags -> [Word]
getWords = filter (not . T.null) . map cleanWord . concatMap (T.words . innerText . pure)

-- чистка слов от неалфавитных знаков, приведение к нижнему регистру
cleanWord :: Word -> Word
cleanWord = T.toLower . T.reverse . T.dropWhile (not . isAlpha) . T.reverse . T.dropWhile (not . isAlpha)

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