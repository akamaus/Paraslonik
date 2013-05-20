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

import Data.Digest.Pure.MD5(md5)
import Data.Strings(lazyBytes)

import System.Directory
import System.FilePath

getContentType :: URI -> IO (Fallible String)
getContentType uri = do
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
      _ -> return $ Left (show r)

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
           _ -> return $ Left (show r)
 `catch` (\(exn :: IOException) -> return $ Left "connection failed")
   where determine_encoding resp = do
           str <- findHeader HdrContentType resp
           let charset = "charset="
           res <- find (charset `isPrefixOf`) . tails . map toLower $ str
           let enc_str = map (\c -> case c of '-' -> '_'; _ -> c) . drop (length charset) $ res
           encodingFromStringExplicit enc_str

-- скачивает страницу, или возвращает из кэша
getPage :: URI -> IO (Fallible Document)
getPage url = do
  createDirectoryIfMissing False cache_dir
  let url_filename = show . md5 . lazyBytes . show $ url
      path = cache_dir </> url_filename
  cached <- doesFileExist path
  case cached of
    True -> liftM Right (readFile path)
    False -> do res <- downloadURL url
                case res of
                  Left _ -> return ()
                  Right contents -> writeFile path contents
                return res
 where cache_dir = "pages-cache"

