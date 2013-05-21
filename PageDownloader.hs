{-# LANGUAGE ScopedTypeVariables #-}

-- Модуль, скачивающий и кэширующий отдельные страницы
module PageDownloader(getPage, getContentType, URI) where

import Common
import PageProcessor

import Network.HTTP
import Network.URI

import Data.Encoding

import Data.List(tails,find,isPrefixOf)
import Data.Char(toLower)

import Prelude hiding (catch)

import Control.Exception (IOException, catch)
import Control.Monad(liftM)

import System.Directory
import System.FilePath

-- Выполняет заданный http запрос, выполняет одно из двух действий в зависимости от кода успеха
httpRequest :: RequestMethod -> URI -> (URI -> IO (Fallible a)) -> (Response String -> Fallible a) -> IO (Fallible a)
httpRequest method url onRedirect onSuccess = do
  resp <- simpleHTTP ((mkRequest method url) :: Request String)
  case resp of
    Left x -> return $ Left ("Error connecting: " ++ show x)
    Right r -> case rspCode r of
      (2,_,_) -> return $ onSuccess r
      (3,_,_) ->
             case findHeader HdrLocation r of
               Nothing -> return $ Left $ "can't find redirect location" ++ (show r)
               Just str -> case parseURI str of
                 Just uri -> onRedirect uri
                 Nothing -> return $ Left "can't parse redirection url"
      _ -> return $ Left (rspReason r)
 `catch` (\(exn :: IOException) -> return $ Left "connection failed on probing")

-- опряделяет content-type HEAD запросом, позволяет не качать впустую картинки и прочую мелочь
downloadContentType :: URI -> IO (Fallible String)
downloadContentType url = httpRequest HEAD url getContentType extract
 where extract r = case findHeader HdrContentType r of
                     Nothing ->  Left "couldn't get content type"
                     Just str -> Right $ fst $ break (==';') str

-- скачивает страницу, определяет кодировку
downloadURL :: URI -> IO (Fallible Document)
downloadURL url = httpRequest GET url getPage extractContents
 where extractContents r =
         case determine_encoding r of
           Just enc -> case decodeStringExplicit enc (rspBody r) of
             Left err -> Left $ "couldn't decode page:" ++ show err
             Right x ->  Right x
           Nothing -> Left "couldn't determine encoding"
       determine_encoding resp = do
           str <- findHeader HdrContentType resp
           let charset = "charset="
           res <- find (charset `isPrefixOf`) . tails . map toLower $ str
           let enc_str = map (\c -> case c of '-' -> '_'; _ -> c) . drop (length charset) $ res
           encodingFromStringExplicit enc_str

-- получает content-type документа или берет из кэша
getContentType :: URI -> IO (Fallible String)
getContentType url = do
  createDirectoryIfMissing False cacheDir
  let path = cacheDir </> urlToFile url <.> "head"
  cached <- doesFileExist path
  case cached of
    True -> do liftM Right (readFile path)
    False -> do res <- downloadContentType url
                info $ "probed page: " ++ show url
                case res of
                  Left _ -> return ()
                  Right contents -> writeFile path contents
                return res

-- скачивает страницу или возвращает из кэша
getPage :: URI -> IO (Fallible Document)
getPage url = do
  createDirectoryIfMissing False cacheDir
  let path = cacheDir </> urlToFile url
  cached <- doesFileExist path
  case cached of
    True -> do info $ "got page from cache: " ++ show url
               liftM Right (readFile path)
    False -> do res <- downloadURL url
                info $ "downloaded page: " ++ show url
                case res of
                  Left _ -> return ()
                  Right contents -> writeFile path contents
                return res
