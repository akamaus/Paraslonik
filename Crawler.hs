{-# LANGUAGE ScopedTypeVariables #-}

import Network.HTTP
import Text.HTML.TagSoup

import Data.List
import Prelude hiding (catch)
import Control.Exception (IOException, catch)

downloadURL :: String -> IO (Either String String)
downloadURL url =
  do resp <- simpleHTTP (getRequest url)
     case resp of
       Left x -> return $ Left ("Error connecting: " ++ show x)
       Right r -> 
         case rspCode r of
           (2,_,_) -> return $ Right (rspBody r)
           (3,_,_) ->
             -- HTTP Redirect
             case findHeader HdrLocation r of
               Nothing -> return $ Left (show r)
               Just url -> downloadURL url
           _ -> return $ Left (show r)
 `catch` (\(exn :: IOException) -> return $ Left "connection failed")

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