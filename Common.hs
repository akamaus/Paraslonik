{-# LANGUAGE NoMonomorphismRestriction #-}

module Common(module Common,
              (A.<$>), (A.<*>),
              when, unless, liftIO,
              isJust, fromMaybe,
              Text) where

import qualified Control.Applicative as A

import System.IO
import Control.Monad
import Data.Maybe

import Data.Text

import Data.Digest.Pure.MD5(md5)
import Data.Strings(lazyBytes)

import Network.URI
import System.IO.Unsafe
import Control.Concurrent.MVar

import Control.Monad.Trans(liftIO)
import Control.Monad.Error

type ErrorMsg = String
type Fallible = Either ErrorMsg
type Downloader = ErrorT ErrorMsg IO

runDownloader = runErrorT

type Document = Text
type Title = Text
type Word = Text

cacheDir = "pages-cache" -- каталог с кэшем страниц
indexDir = "index-cache" -- каталог с индексами сайтов

{-# NOINLINE consoleMutex #-}
consoleMutex = unsafePerformIO $ newMVar () -- мьютекс, дабы не замешивался отладочный вывод

sync :: IO a -> IO a
sync act = withMVar consoleMutex (\_ -> act)

warn = liftIO . sync . putStrLn
info = liftIO . sync . putStrLn

--debug :: String -> IO ()
debug = const $ return () --info

-- преобразование урла в имя файла, для целей кэширования
urlToFile :: URI -> FilePath
urlToFile = show . md5 . lazyBytes . show

