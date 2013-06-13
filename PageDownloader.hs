{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-- Модуль, скачивающий и кэширующий отдельные страницы
module PageDownloader(getPage, getContentType, URI) where

import Common
import PageProcessor

import Network.HTTP
import Network.URI

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.ByteString as BS

import Data.Encoding
import Data.Encoding.UTF8

import Data.Text.ICU.Convert

import Data.List(tails,find,isPrefixOf)
import Data.Char(toLower)

import Prelude hiding (catch)

import Control.Exception (IOException, catch)
import Control.Monad(liftM)

import System.Directory
import System.FilePath

-- Выполняет заданный http запрос, выполняет одно из двух действий в зависимости от кода успеха
httpRequest :: RequestMethod -> URI -> (URI -> IO (Fallible a)) -> (Response BS.ByteString -> IO (Fallible a)) -> IO (Fallible a)
httpRequest method url onRedirect onSuccess = do
  resp <- simpleHTTP ((mkRequest method url) :: Request BS.ByteString)
  case resp of
    Left x -> return $ Left ("Error connecting to " ++ show url ++ ": " ++ show x)
    Right r -> case rspCode r of
      (2,_,_) -> onSuccess r -- успех, вызываем переданную функцию обработки
      (3,_,_) ->
             case findHeader HdrLocation r of
               Nothing -> return $ Left $ "can't find redirect location" ++ (show r)
               Just str -> let decoded = decodeString UTF8 str
                           in case procLinks url [decoded] of
                 [new] ->  do info $ "link " ++ show url ++ " redirects to " ++ decoded
                              onRedirect new -- редирект, переходим к новой урле
                 _ -> return $ Left $ "can't parse redirection url, " ++ str ++ " on page " ++ show url
      _ -> return $ Left ("http error on " ++ show url ++ " : " ++ rspReason r)
 `catch` (\(exn :: IOException) -> return $ Left "connection failed on probing")

-- опряделяет content-type HEAD запросом, позволяет не качать впустую картинки и прочую мелочь
downloadContentType :: URI -> IO (Fallible Word)
downloadContentType url = httpRequest HEAD url getContentType (return . extract)
 where extract r = case findHeader HdrContentType r of -- извлекаем заголовок
                     Nothing ->  Left "couldn't get content type"
                     Just str -> Right $ T.pack $ fst $ break (==';') str

-- скачивает страницу, определяет кодировку
downloadURL :: URI -> IO (Fallible Document)
downloadURL url = httpRequest GET url getPage extractContents
 where extractContents r =
         case getEncoding r of
           Just enc -> do
             conv <- open (T.unpack enc) Nothing
             return $ Right $ toUnicode conv (rspBody r)
           Nothing -> return $ Left "couldn't determine encoding"

-- определяем кодировку по заголовку Content-type, там бывает поле charset
--getEncoding :: IO Text
getEncoding resp = do
  str <- T.pack <$> findHeader HdrContentType resp
  let charset = "charset="
  res <- find (charset `T.isPrefixOf`) . T.tails . T.toLower $ str
  let enc_str = T.map (\c -> case c of '-' -> '_'; _ -> c) . T.drop (T.length charset) $ res -- ищем charset в строке
  return enc_str

-- получает content-type документа или берет из кэша
getContentType :: URI -> IO (Fallible Word)
getContentType url = do
  createDirectoryIfMissing False cacheDir
  let path = cacheDir </> urlToFile url <.> "head"
  cached <- doesFileExist path
  case cached of
    True -> do liftM Right (T.readFile path)
    False -> do res <- downloadContentType url
                info $ "probed page: " ++ show url
                case res of
                  Left _ -> return ()
                  Right contents -> T.writeFile path contents
                return res

-- скачивает страницу или возвращает из кэша
getPage :: URI -> IO (Fallible Document)
getPage url = do
  createDirectoryIfMissing False cacheDir
  let path = cacheDir </> urlToFile url
  cached <- doesFileExist path
  case cached of
    True -> do info $ "got page from cache: " ++ show url
               liftM Right (T.readFile path)
    False -> do res <- downloadURL url
                info $ "downloaded page: " ++ show url
                case res of
                  Left _ -> return ()
                  Right contents -> T.writeFile path contents
                return res
