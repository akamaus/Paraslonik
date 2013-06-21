{-# LANGUAGE TemplateHaskell #-}
module Types(module Types, Text,
             ask, asks,
             modify
             ) where

import Control.Monad.Error
import Control.Monad.Reader
import Text.Regex.TDFA

import Data.Text

import Control.Concurrent.MVar

import Data.Label

type Document = Text
type Title = Text
type Word = Text

type Depth = Int

instance Show Regex where -- for debug
  show _ = "<regex>"

data IndexRestrictions = IndexRestrictions {irNumWorkers :: Int,
                                            irWaitTime :: Maybe Int,
                                            irAgent :: String,
                                            irDepth :: Maybe Depth, irMaxPages :: Maybe Int,
                                            irDomain :: Maybe String,
                                            irQueryTest :: Maybe Regex,
                                            irIgnoreQueryTest :: Maybe Regex
                                            } deriving Show

data Statistics = Statistics {
  _sPagesAnalyzed :: Integer,
  _sPagesGotFromCache :: Integer,
  _sBytesAnalyzed :: Integer,
  _sBytesGotFromCache :: Integer,
  _sLinksAnalyzed :: Integer,
  _sRedirectsFollowed :: Integer
  } deriving Show

mkLabels [''Statistics]

emptyStats = Statistics 0 0 0 0 0 0

data DownloaderEnv = DownloaderEnv { deRestrictions :: IndexRestrictions, -- various parameters of crawler setup
                                     deStats :: MVar Statistics, -- updated by all the workers concurrently
                                     deWaitSemaphor :: MVar () -- consumed on every HTTP request
                                   }

type ErrorMsg = String
type Fallible = Either ErrorMsg

type Downloader = ReaderT DownloaderEnv (ErrorT ErrorMsg IO)

runDownloader d r = runErrorT $ runReaderT d r

