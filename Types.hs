module Types(module Types, Text, ask) where

import Control.Monad.Error
import Control.Monad.Reader
import Text.Regex.TDFA

import Data.Text

type ErrorMsg = String
type Fallible = Either ErrorMsg
type Downloader = ReaderT IndexRestrictions (ErrorT ErrorMsg IO)

runDownloader d r = runErrorT $ runReaderT d r

type Document = Text
type Title = Text
type Word = Text

type Depth = Int
data IndexRestrictions = IndexRestrictions {irNumWorkers :: Int,
                                            irDepth :: Maybe Depth, irMaxPages :: Maybe Int,
                                            irDomain :: Maybe String,
                                            irQueryTest :: Maybe Regex,
                                            irIgnoreQueryTest :: Maybe Regex
                                            } deriving Show
instance Show Regex where -- for debug
  show _ = "<regex>"
