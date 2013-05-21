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
  page_indexes <- crawleSite page_processor numPages url
  page_indexes' <- filterM (\(url, res) -> case res of
                               Left err -> warn err >> return False
                               Right res -> return True) page_indexes >>= mapM (\(u,r) -> case r of Right x -> return (u,x))
  let index = indexPages page_indexes'
  return index

-- обработчик отдельных страниц
page_processor :: Processor PageIndex
page_processor (Left err) = (Left err)
page_processor (Right tags) = Right (indexContent . getWords $ tags)

usage = unlines [ "Usage:",
                  "crawler.exe <site url> [<query words>]"
                ]
