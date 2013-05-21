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

-- опряделяет content-type HEAD запросом, позволяет не качать впустую картинки и прочую мелочь
downloadContentType :: URI -> IO (Fallible String)
downloadContentType uri = do
  resp <- simpleHTTP ((mkRequest HEAD uri) :: Request String)
  case resp of
    Left x -> return $ Left ("Error connecting: " ++ show x)
    Right r -> case rspCode r of
      (2,_,_) -> do let mstr = findHeader HdrContentType r
                    case mstr of
                      Nothing -> return $ Left "couldn't get content type"
                      Just str -> return $ Right $ fst $ break (==';') str
      (3,_,_) ->
             case findHeader HdrLocation r of
               Nothing -> return $ Left $ "can't find redirect location" ++ (show r)
               Just str -> case parseURI str of
                 Just uri -> getContentType uri
                 Nothing -> return $ Left "can't parse redirection url"
      _ -> return $ Left (rspReason r)
 `catch` (\(exn :: IOException) -> return $ Left "connection failed on probing")
-- скачивает страницу, определяет кодировку
downloadURL :: URI -> IO (Fallible Document)
downloadURL uri =
  do resp <- simpleHTTP (mkRequest GET uri)
     case resp of
       Left x -> return $ Left ("Error connecting: " ++ show x)
       Right r ->
         case rspCode r of
           (2,_,_) -> case determine_encoding r of
             Just enc -> case decodeStringExplicit enc (rspBody r) of
               Left err -> return $ Left $ "couldn't decode page:" ++ show err
               Right x -> return $ Right x
             Nothing -> return $ Left "couldn't determine encoding"
           (3,_,_) ->
             -- HTTP Redirect
             case findHeader HdrLocation r of
               Nothing -> return $ Left (show r)
               Just str -> case parseURI str of
                 Just uri -> getPage uri
                 Nothing -> return $ Left "can't parse redirection url"
           _ -> return $ Left (rspReason r)
 `catch` (\(exn :: IOException) -> return $ Left "connection failed")
   where determine_encoding resp = do
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
