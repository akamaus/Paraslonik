{-# LANGUAGE ScopedTypeVariables #-}

import Network.HTTP
import Network.URI

import Data.Encoding

import Text.HTML.TagSoup

import Data.List
import Data.Char(toLower)

import Prelude hiding (catch)
import Control.Exception (IOException, catch)

import Debug.Trace

-- скачивает URL, определяет кодировку
downloadURL :: URI -> IO (Either String String)
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
                 Just uri -> downloadURL uri
           _ -> return $ Left (show r)
 `catch` (\(exn :: IOException) -> return $ Left "connection failed")
   where determine_encoding resp = do
           str <- findHeader HdrContentType resp
           let charset = "charset="
           res <- find (charset `isPrefixOf`) . tails . map toLower $ str
           let enc_str = map (\c -> case c of '-' -> '_'; _ -> c) . drop (length charset) $ res
           encodingFromStringExplicit enc_str


parse = dropTags ["script", "style"] . parseTags

getLinks = map (fromAttrib "href") . filter (~== "<a href>")
getWords = words . innerText

dropTags :: (Eq str) => [str] -> [Tag str] -> [Tag str]
dropTags labels [] = []
dropTags labels (tag:rest) =
  case find (flip isTagOpenName tag) labels of
    Nothing -> tag : dropTags labels rest
    Just target -> skipTo target rest
  where
    skipTo target [] = []
    skipTo target (tag:rest)
      | isTagCloseName target tag = dropTags labels rest
      | otherwise = skipTo target rest