-------------------
-- 07. Modules
-- http://learnyouahaskell.com/modules
-------------------

import Data.List
import Data.Function
import Data.Char
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

sumPolynomials :: [Int]
sumPolynomials = map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]

-- takeWhile (> 3) [6,5,4,3,2,1,2,3,4,5]
-- takeWhile (/=' ') "This is a sentence"

-- find the first stock price entry (with date) that is over 1000
stockPrice :: (Float, Int, Int, Int)
stockPrice =
  let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
  in head . dropWhile (\(val,y,m,d) -> val < 1000) $ stock

-- break (==4) [1,2,3,4,5,6,7]
-- span (/=4) [1,2,3,4,5,6,7]

-- map (\l@(x:xs) -> (x, length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]

isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf' needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- find :: (a -> Bool) -> [a] -> Maybe a

-- groupBy (\x y -> (x > 0) == (y > 0)) values
-- # group by equality (==) based on whether the values are more than 0
-- groupBy (\x y -> ((==) `on` (> 0)) values

words' :: String -> [String]
words' = filter (not . any isSpace) . groupBy ((==) `on` isSpace)

-- Caesar cipher
encode :: Int -> String -> String
encode shift = map (chr . (+ shift) . ord)

decode :: Int -> String -> String
decode shift = encode (negate shift)

phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

findByKey :: (Eq k) => k -> [(k,v)] -> Maybe v
{-- #1
findByKey key = snd . head . filter (\(k,v) -> key == k) -- unsafe
--}
{-- #2
findByKey key [] = Nothing
findByKey key ((k,v):xs) =
  if key == k
  then Just v
  else findByKey key xs

 -- #3 (tail-recursive)
findByKey' key xs = _findByKey Nothing xs
  where
    _findByKey result [] = result
    _findByKey result ((k,v):xs) =
      if key == k
      then Just v
      else _findByKey result xs
--}
-- ^ this is fold so it's better written as:
-- #4
findByKey key = foldr (\(k,v) result -> if key == k then Just v else result) Nothing

