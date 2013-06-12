{-# LANGUAGE TemplateHaskell #-}
-- интерфейс программы
module Main where

import Common
import Crawler
import PageIndex
import Query
import PageProcessor

import Network.URI

import System.Environment(getArgs)

import System.Directory
import System.FilePath

import Data.Binary
import Data.DeriveTH

$(derives [makeBinary] [''URIAuth, ''URI, ''GlobalData])

main = do
  args <- getArgs
  case args of
    (site:command:query) -> do
      case parseURI site of
        Nothing -> warn "can't parse site url" >> warn usage
        Just url -> case command of
          "reindex" -> do -- повторная индексация
            getIndex url False
            return ()
          "search" -> do -- поиск слов
            index <- getIndex url True
            putStrLn "Search results:"
            mapM_ print $ findPages index (map cleanWord query)
          "show" -> do -- вывод базы
            index <- getIndex url True
            printDatabase index
    _ -> warn usage

-- получаем главный индекс (либо читаем с диска, либо строим)
getIndex :: URI -> Bool ->  IO GlobalData
getIndex url use_cache = do
  createDirectoryIfMissing False indexDir
  let path = indexDir </> urlToFile url
  cached <- doesFileExist path
  case use_cache && cached of
    True -> do decodeFile path
    False -> do index <- buildIndex url
                encodeFile path index
                return index

-- строим индекс
buildIndex :: URI -> IO GlobalData
buildIndex url = do
  runner <- mkSiteCrawler numPages url
  runner emptyDatabase (\(u,ts) -> (u, pageProcessor ts)) addPage

-- обработчик отдельных страниц
pageProcessor :: Tags -> PageData
pageProcessor tags = PageData (indexContent . getWords $ tags) (getTitle tags)

usage = unlines [ "Usage:"
                , "crawler.exe <site url> reindex"
                , "crawler.exe <site url> search <query words>"
                , "crawler.exe <site url> show"
                ]
