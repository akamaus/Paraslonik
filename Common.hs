module Common where

import System.IO

type ErrorMsg = String
type Fallible = Either ErrorMsg

type Document = String
type Word = String

warn = hPutStrLn stderr
info = warn

debug :: String -> IO ()
debug = const $ return () --info

