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
    (site:query) -> do
      case parseURI site of
        Nothing -> warn "can't parse site url" >> warn usage
        Just url -> do
          index <- getIndex url
          putStrLn "Search results:"
          mapM_ print $ findPages index query
    _ -> warn usage

-- получаем главный индекс (либо читаем с диска, либо строим)
getIndex :: URI -> IO GlobalIndex
getIndex url = do
  createDirectoryIfMissing False indexDir
  let path = indexDir </> urlToFile url
  cached <- doesFileExist path
  case cached of
    True -> do decodeFile path
    False -> do index <- buildIndex url
                encodeFile path index
                return index

-- строим индекс
buildIndex :: URI -> IO GlobalIndex
buildIndex url = do
  results <- crawleSite numPages url
  epages <- filterM (\(url, res) -> case res of
                       Left err -> warn err >> return False
                       Right res -> return True) results
  let pages = map (\(u,r) -> case r of Right x -> (u,x)) epages
      page_stats = map (\(u,t) -> trace ("processing " ++ show u) $ (u, pageProcessor t)) pages
      index = indexPages page_stats
  return index

-- обработчик отдельных страниц
pageProcessor :: Tags -> PageIndex
pageProcessor = indexContent . getWords

usage = unlines [ "Usage:",
                  "crawler.exe <site url> [<query words>]"
                ]
