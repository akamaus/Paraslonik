module Tests where

import Common
import PageIndex
import Query

import Network.URI

Just u1 = parseURI "http://test1.ru"
Just u2 = parseURI "http://test2.ru"
Just u3 = parseURI "http://test3.ru"

cont1 = "Yesterday I went to the school"
cont2 = "Yesterday he swimmed the pool"
cont3 = "Today I am swimming in the pool"

index = indexPages $ zip [u1,u2,u3] (map (indexContent . words) [cont1, cont2, cont3])

tests = do
  print $ findPages index ["I"]
  print $ findPages index ["I", "school"]
  print $ findPages index ["I", "school", "Yesterday"]
  print $ findPages index ["I", "school", "Yesterday", "pool"]
