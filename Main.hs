{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
-- интерфейс программы
module Main where

import Common hiding(info)
import Crawler
import PageIndex
import Query
import PageProcessor

import qualified Data.Label as L
import qualified Data.Text as T

import Network.URI

import System.Environment(getArgs)
import Options.Applicative

import System.Directory
import System.FilePath

import Data.Binary(Binary,encodeFile, decodeFile, put, get)
import Data.Text.Binary

import Text.Regex.TDFA
import Text.Regex.Base

import Data.DeriveTH

$(derives [makeBinary] [''URIAuth, ''URI, ''GlobalData])

data CmdLine = CmdLine { clUrl :: URI
                       , clCmd :: Command
                       } deriving Show

data Command = CmdReindex IndexRestrictions | CmdSearch [Word] | CmdShow deriving Show

main = do
  CmdLine url command <- execParser cmdParser
  case command of
    CmdReindex restrictions -> do -- повторная индексация
      stats <- storeIndex restrictions url
      printStats stats
    CmdSearch query -> do -- поиск слов
      withIndex url $ \index -> do
        printInfo "Search results:"
        mapM_ print $ findPages index query
    CmdShow -> do -- вывод базы
      withIndex url printDatabase
 where withIndex url act = do
         mindex <- readIndex url
         case mindex of
           Nothing -> err "Build index first"
           Just index -> act index
       printStats stats = mapM_ (\(caption, getter) -> printInfo $ caption ++ " " ++ show (L.get getter stats))
                            [ ("Pages analyzed", sPagesAnalyzed),
                              ("Pages got from cache", sPagesGotFromCache),
                              ("Bytes analyzed", sBytesAnalyzed),
                              ("Bytes got from cache", sBytesGotFromCache),
                              ("Links analyzed", sLinksAnalyzed),
                              ("Redirects followed", sRedirectsFollowed)
                            ]
cmdParser = info (CmdLine <$> site_p <*> command_p) description
 where site_p = argument (\s -> str s >>= parseURI) (metavar "SITE")
       command_p = subparser (
         command "reindex" (info (CmdReindex <$> index_restrictions) ( progDesc "Reindex site"))
         <> command "show" (info (pure CmdShow) ( progDesc "Show database"))
         <> command "search" (info (CmdSearch <$>
                                    many ((cleanWord . T.pack) <$> argument str (metavar "QUERY")) )
                              (progDesc "Search database")))
       description = fullDesc <> progDesc "A site crawler and query engine"
       index_restrictions = IndexRestrictions <$>
         option (long "num-workers" <> short 'n' <> value 10 <> metavar "WORKERS") <*>
         optional ((\(s :: Float) -> round $ s * 1000000) <$> option (long "wait" <> short 'w' <> metavar "SECONDS")) <*>
         strOption (long "agent-string" <> short 'a' <> value "Paraslonik" <> metavar "AGENT") <*>
         optional (option $ long "depth" <> short 'd' <> metavar "DEPTH") <*>
         optional (option $ long "max-pages" <> short 'p' <> metavar "PAGES") <*>
         optional (strOption $ long "domain" <> metavar "DOMAIN") <*>
         optional ( (makeRegex :: String -> Regex) <$> (strOption $ long "query" <> metavar "REGEX")) <*>
         optional ( (makeRegex :: String -> Regex) <$> (strOption $ long "ignore-query" <> metavar "REGEX"))

-- получаем главный индекс (либо читаем с диска, либо строим)
storeIndex :: IndexRestrictions -> URI -> IO Statistics
storeIndex restrictions url = do
  createDirectoryIfMissing False indexDir
  let path = indexDir </> urlToFile url
  (index,stats) <- buildIndex restrictions url
  encodeFile path index
  return stats

readIndex :: URI -> IO (Maybe GlobalData)
readIndex url = do
  let path = indexDir </> urlToFile url
  cached <- doesFileExist path
  case cached of
    True -> Just <$> decodeFile path
    False -> return Nothing

-- строим индекс
buildIndex :: IndexRestrictions -> URI -> IO (GlobalData, Statistics)
buildIndex restrictions url = do
  (runner,stats_var) <- mkSiteCrawler restrictions url
  index <- runner emptyDatabase (\(u,ts) -> (u, pageProcessor ts)) addPage
  stats <- readMVar stats_var
  return (index,stats)


-- обработчик отдельных страниц
pageProcessor :: Tags -> PageData
pageProcessor tags = PageData (indexContent . getWords $ tags) (getTitle tags)

usage_msg = unlines [ "Usage:"
                , "crawler.exe <site url> reindex"
                , "crawler.exe <site url> search <query words>"
                , "crawler.exe <site url> show"
                ]
