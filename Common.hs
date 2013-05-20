module Common where

import System.IO

type ErrorMsg = String
type Document = String
type Fallible = Either ErrorMsg

warn = hPutStrLn stderr
info = warn

debug :: String -> IO ()
debug = const $ return () --info

