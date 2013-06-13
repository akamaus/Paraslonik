module Common(module Common, (A.<$>), Text) where

import qualified Control.Applicative as A

import System.IO

import Data.Text

import Data.Digest.Pure.MD5(md5)
import Data.Strings(lazyBytes)

import Network.URI
import System.IO.Unsafe
import Control.Concurrent.MVar

type ErrorMsg = String
type Fallible = Either ErrorMsg

type Document = Text
type Title = Text
type Word = Text

numPages   = 10000 :: Int -- глубина обхода
numWorkers = 10 :: Int -- количество потоков

cacheDir = "pages-cache" -- каталог с кэшем страниц
indexDir = "index-cache" -- каталог с индексами сайтов

{-# NOINLINE consoleMutex #-}
consoleMutex = unsafePerformIO $ newMVar () -- мьютекс, дабы не замешивался отладочный вывод

sync :: IO a -> IO a
sync act = withMVar consoleMutex (\_ -> act)

warn = sync . putStrLn
info = sync . putStrLn

debug :: String -> IO ()
debug = const $ return () --info

-- преобразование урла в имя файла, для целей кэширования
urlToFile :: URI -> FilePath
urlToFile = show . md5 . lazyBytes . show

