{-# LANGUAGE TemplateHaskell #-}

module Main where

import Common
import Crawler
import PageIndex
import Query
import PageProcessor

import Network.URI

import System.Environment(getArgs)
import Control.Monad(filterM)

import System.Directory
import System.FilePath

import Data.Binary
import Data.DeriveTH

import Debug.Trace

$(derives [makeBinary] [''URIAuth, ''URI])

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
            printGlobalIndex index
    _ -> warn usage

-- получаем главный индекс (либо читаем с диска, либо строим)
getIndex :: URI -> Bool ->  IO GlobalIndex
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
buildIndex :: URI -> IO GlobalIndex
buildIndex url = do
  runner <- mkSiteCrawler numPages url
  runner emptyGlobalIndex (\(u,ts) -> (u, pageProcessor ts)) addPageToIndex

-- обработчик отдельных страниц
pageProcessor :: Tags -> PageIndex
pageProcessor = indexContent . getWords

usage = unlines [ "Usage:",
                  "crawler.exe <site url> [<query words>]"
                ]
