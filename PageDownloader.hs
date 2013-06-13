{-# LANGUAGE ScopedTypeVariables #-}

-- Модуль, скачивающий и кэширующий отдельные страницы
module PageDownloader(getPage, getContentType, URI) where

import Common
import PageProcessor

import Network.HTTP
import Network.URI

import Data.Encoding
import Data.Encoding.UTF8

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
    Left x -> return $ Left ("Error connecting to " ++ show url ++ ": " ++ show x)
    Right r -> case rspCode r of
      (2,_,_) -> return $ onSuccess r -- успех, вызываем переданную функцию обработки
      (3,_,_) ->
             case (findHeader HdrLocation r, getEncoding r) of
               (Nothing,_) -> return $ Left $ "can't find redirect location" ++ (show r)
               (Just str, Just enc) -> let decoded = decodeString UTF8 str
                                       in case procLinks url [decoded] of
                 [new] ->  do info $ "link " ++ show url ++ " redirects to " ++ decodeString UTF8 str
                              onRedirect new -- редирект, переходим к новой урле
                 _ -> return $ Left $ "can't parse redirection url, " ++ str ++ " on page " ++ show url
      _ -> return $ Left ("http error on " ++ show url ++ " : " ++ rspReason r)
 `catch` (\(exn :: IOException) -> return $ Left "connection failed on probing")

-- опряделяет content-type HEAD запросом, позволяет не качать впустую картинки и прочую мелочь
downloadContentType :: URI -> IO (Fallible String)
downloadContentType url = httpRequest HEAD url getContentType extract
 where extract r = case findHeader HdrContentType r of -- извлекаем заголовок
                     Nothing ->  Left "couldn't get content type"
                     Just str -> Right $ fst $ break (==';') str

-- скачивает страницу, определяет кодировку
downloadURL :: URI -> IO (Fallible Document)
downloadURL url = httpRequest GET url getPage extractContents
 where extractContents r =
         case getEncoding r of
           Just enc -> case decodeStringExplicit enc $ rspBody r of
             Left err -> Left $ "couldn't decode page:" ++ show err
             Right x ->  Right x
           Nothing -> Left "couldn't determine encoding"

-- определяем кодировку по заголовку Content-type, там бывает поле charset
getEncoding resp = do
  str <- findHeader HdrContentType resp
  let charset = "charset="
  res <- find (charset `isPrefixOf`) . tails . map toLower $ str
  let enc_str = map (\c -> case c of '-' -> '_'; _ -> c) . drop (length charset) $ res -- ищем charset в строке
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
