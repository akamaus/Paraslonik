{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-- Модуль, скачивающий и кэширующий отдельные страницы
module PageDownloader(getPage, getContentType, URI) where

import Common
import PageProcessor

import Prelude hiding(id,(.),catch)
import Control.Category(id, (.))

import Network.HTTP
import Network.URI
import Network.Stream

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.ByteString as BS

import Data.Encoding
import Data.Encoding.UTF8

import Data.Text.ICU.Convert

import Data.List(tails,find,isPrefixOf)
import Data.Char(toLower)

import Control.Exception (IOException, catch)
import Control.Monad(liftM)

import System.Directory
import System.FilePath

-- Выполняет заданный http запрос, выполняет одно из двух действий в зависимости от кода успеха
httpRequest :: RequestMethod -> URI -> (URI -> Downloader a) -> (Response BS.ByteString -> Downloader a) -> Downloader a
httpRequest method url onRedirect onSuccess = do
  resp <- liftIO $ simpleHTTP ((mkRequest method url) :: Request BS.ByteString) `catch` (\(exn :: IOException) -> return $ Left $ ErrorMisc "download failed")
  case resp of
    Left x -> fail $ "Error connecting to " ++ show url ++ ": " ++ show x
    Right r -> case rspCode r of
      (2,_,_) -> onSuccess r -- успех, вызываем переданную функцию обработки
      (3,_,_) ->
             case findHeader HdrLocation r of
               Nothing -> fail $ "can't find redirect location" ++ show r
               Just str -> let decoded = decodeString UTF8 str
                           in case procLinks url [decoded] of
                 [new] ->  do info $ "link " ++ show url ++ " redirects to " ++ decoded
                              stats <- asks deStats
                              liftIO $ modifyMVar_ stats (return . modify sRedirectsFollowed (+1))
                              onRedirect new -- редирект, переходим к новой урле
                 _ -> fail $ "can't parse redirection url, " ++ str ++ " on page " ++ show url
      _ -> fail $ "http error on " ++ show url ++ " : " ++ rspReason r

-- опряделяет content-type HEAD запросом, позволяет не качать впустую картинки и прочую мелочь
downloadContentType :: URI -> Downloader Word
downloadContentType url = httpRequest HEAD url getContentType extract
 where extract r = case findHeader HdrContentType r of -- извлекаем заголовок
                     Nothing ->  fail "couldn't get content type"
                     Just str -> return $ T.pack $ fst $ break (==';') str

-- скачивает страницу, определяет кодировку
downloadURL :: URI -> Downloader Document
downloadURL url = httpRequest GET url getPage extractContents
 where extractContents r =
         case getEncoding r of
           Just enc -> liftIO $ do
             conv <- open (T.unpack enc) Nothing
             return $ toUnicode conv (rspBody r)
           Nothing -> fail "couldn't determine encoding"

-- определяем кодировку по заголовку Content-type, там бывает поле charset
--getEncoding :: IO Text
getEncoding resp = do
  str <- T.pack <$> findHeader HdrContentType resp
  let charset = "charset="
  res <- find (charset `T.isPrefixOf`) . T.tails . T.toLower $ str
  let enc_str = T.map (\c -> case c of '-' -> '_'; _ -> c) . T.drop (T.length charset) $ res -- ищем charset в строке
  return enc_str

-- получает content-type документа или берет из кэша
getContentType :: URI -> Downloader Word
getContentType url = do
  liftIO $ createDirectoryIfMissing False cacheDir
  let path = cacheDir </> urlToFile url <.> "head"
  cached <- liftIO $ doesFileExist path
  ct <- case cached of
    True -> do liftIO $ T.readFile path
    False -> do res <- downloadContentType url
                info $ "probed page: " ++ show url
                liftIO $ T.writeFile path res
                return res
  stats <- asks deStats
  liftIO $ modifyMVar_ stats (return . modify sLinksAnalyzed (+1))
  return ct

-- скачивает страницу или возвращает из кэша
getPage :: URI -> Downloader Document
getPage url = do
  liftIO $ createDirectoryIfMissing False cacheDir
  let path = cacheDir </> urlToFile url
  cached <- liftIO $ doesFileExist path
  (text, statsUp) <- case cached of
    True -> do info $ "got page from cache: " ++ show url
               text <- liftIO $ T.readFile path
               return (text, modify sPagesGotFromCache (+1) . modify sBytesGotFromCache (+ fi (T.length text)))
    False -> do text <- downloadURL url
                info $ "downloaded page: " ++ show url
                liftIO $  T.writeFile path text
                return (text, id)
  stats <- asks deStats
  liftIO $ modifyMVar_ stats (return . modify sPagesAnalyzed (+1) . modify sBytesAnalyzed (+ fi (T.length text)) . statsUp)
  return text
