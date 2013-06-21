{-# LANGUAGE NoMonomorphismRestriction #-}

module Common(module Common, module Types,
              (A.<$>), (A.<*>),
              when, unless, liftIO,
              isJust, fromMaybe,
             ) where

import Types

import qualified Control.Applicative as A

import System.IO
import Control.Monad
import Data.Maybe

import Data.Digest.Pure.MD5(md5)
import Data.Strings(lazyBytes)

import Network.URI
import System.IO.Unsafe
import Control.Concurrent.MVar

import Control.Monad.Trans(liftIO)

cacheDir = "pages-cache" -- каталог с кэшем страниц
indexDir = "index-cache" -- каталог с индексами сайтов

{-# NOINLINE consoleMutex #-}
consoleMutex = unsafePerformIO $ newMVar () -- мьютекс, дабы не замешивался отладочный вывод

sync :: IO a -> IO a
sync act = withMVar consoleMutex (\_ -> act)

err = liftIO . sync . putStrLn
warn = liftIO . sync . putStrLn
info = liftIO . sync . putStrLn

printInfo = info

--debug :: String -> IO ()
debug = const $ return () --info

-- преобразование урла в имя файла, для целей кэширования
urlToFile :: URI -> FilePath
urlToFile = show . md5 . lazyBytes . show

fi = fromIntegral