{-# LANGUAGE TemplateHaskell #-}
-- интерфейс программы
module Main where

import Common hiding(info)
import Crawler
import PageIndex
import Query
import PageProcessor

import qualified Data.Text as T

import Network.URI

import System.Environment(getArgs)
import Options.Applicative

import System.Directory
import System.FilePath

import Data.Binary(Binary,encodeFile, decodeFile, put, get)
import Data.Text.Binary

import Data.DeriveTH

$(derives [makeBinary] [''URIAuth, ''URI, ''GlobalData])

data CmdLine = CmdLine { clUrl :: URI
                       , clCmd :: Command
                       } deriving Show

data Command = CmdReindex | CmdSearch [Word] | CmdShow deriving Show

main = do
  CmdLine url command <- execParser cmdParser
  case command of
    CmdReindex -> do -- повторная индексация
      getIndex url False
      return ()
    CmdSearch query -> do -- поиск слов
      index <- getIndex url True
      putStrLn "Search results:"
      mapM_ print $ findPages index query
    CmdShow -> do -- вывод базы
      index <- getIndex url True
      printDatabase index

cmdParser = info (CmdLine <$> site_p <*> command_p) description
 where site_p = argument (\s -> str s >>= parseURI) (metavar "SITE")
       command_p = subparser (
         command "reindex" (info (pure CmdReindex) ( progDesc "Reindex site"))
         <> command "show" (info (pure CmdShow) ( progDesc "Show database"))
         <> command "search" (info (CmdSearch <$>
                                    many ((cleanWord . T.pack) <$> argument str (metavar "QUERY")) )
                              (progDesc "Search database")))
       description = fullDesc <> progDesc "A site crawler and query engine"


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

usage_msg = unlines [ "Usage:"
                , "crawler.exe <site url> reindex"
                , "crawler.exe <site url> search <query words>"
                , "crawler.exe <site url> show"
                ]
