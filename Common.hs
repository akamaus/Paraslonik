module Common where

import System.IO

import Data.Digest.Pure.MD5(md5)
import Data.Strings(lazyBytes)

import Network.URI

type ErrorMsg = String
type Fallible = Either ErrorMsg

type Document = String
type Word = String

cache_dir = "pages-cache"
index_dir = "index-cache"

warn = hPutStrLn stderr
info = warn

debug :: String -> IO ()
debug = const $ return () --info

urlToFile :: URI -> FilePath
urlToFile = show . md5 . lazyBytes . show