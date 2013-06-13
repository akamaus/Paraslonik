-- Модуль, обрабатывающий Html страницу
module PageProcessor where

import Common

import Network.URI

import Data.List(find, intercalate)
import Text.HTML.TagSoup
import Data.Maybe(fromMaybe, catMaybes)
import Control.Applicative
import Data.Function(on)

type Tags = [Tag String]

-- Разбирает Html в поток тегов
parseHtml = dropTags ["script", "style"] . parseTags

-- Читает ссылки из потока тегов, приводит их к одному виду, выбрасывает внешние
getLinks :: URI -> Tags -> [URI]
getLinks site tags = procLinks site $ map (fromAttrib "href") . filter (~== "<a href>") $ tags

procLinks :: URI -> [String] -> [URI]
procLinks site =
  map normalizeUrl . filter (on (==) (fmap uriRegName . uriAuthority) site) .
  map (relativeTo `flip` site) .
  catMaybes . map (parseURIReference . (\u -> if all isAllowedInURI u then u else escapeURIString isUnescapedInURI u))

normalizeUrl u = hidePort $ u {uriQuery = "", uriFragment = ""}
  where hidePort u | uriScheme u == "http:" = case uriAuthority u of
          Just auth | uriPort auth == ":80" -> u {uriAuthority = Just auth {uriPort = ""}}
          _ -> u
        hidePort u = u

-- получаем заголовок страницы
getTitle :: Tags -> Title
getTitle tags = let title = cleanWord . innerText . takeWhile (not . isTagCloseName  "title") . dropWhile (not . isTagOpenName "title") $ tags
                in length title `seq` title
-- Читаем слова из потока тегов, чистим их от мусора
getWords :: Tags -> [Word]
getWords = filter (not . null) . map cleanWord . concatMap (words . innerText . pure)

-- чистка слов от неалфавитных знаков, приведение к нижнему регистру
cleanWord = map toLower . reverse . dropWhile (not . isAlpha) . reverse . dropWhile (not . isAlpha)

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