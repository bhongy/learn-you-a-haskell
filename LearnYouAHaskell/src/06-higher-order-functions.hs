-------------------
-- 06. Higher order functions
-- http://learnyouahaskell.com/higher-order-functions
-------------------

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- infix operator can also be partially applied by wrapping with ()
-- and supplies the parameter on the side that needs to be partially applied
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

subtractOne :: Integer -> Integer
subtractOne = (subtract 1)

-- need to wrap in paren because `->` is right associative
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x
-- flip' f = \x y -> f y x

-- map (map (^2)) [[1,2], [3,4], [5,6,7]]
-- filter (>3) [1,2,3,4,5]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (> x) xs)
  in smallerSorted ++ (x : biggerSorted)

largestDivisible :: (Integral a) => a
largestDivisible = head (filter predicate [100000, 99999..])
  where predicate x = x `mod` 3829 == 0

{--
oddSquareSum = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))
oddSquareSum = sum (takeWhile (< 10000) [n^2 | n <- [1..], odd (n^2)])
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
--}
oddSquareSum =
  let oddSquare = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (< 10000) oddSquare
  in sum belowLimit

-- Collatz sequences
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15
-- numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- left fold
sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldl (\acc y -> if x == y then True else acc) False

map' :: (a -> b) -> [a] -> [b]
-- use `:` is much more efficient than `++`, hence foldr
map' f = foldr (\x acc -> f x : acc) []

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
-- reverse ' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
-- 1) get sqrt of all integers
-- 2) sum them all up
-- 3) take while the progress of the sum less than 1000
-- 4) + 1 for the "next" element that makes the sum exceeds 1000
-- ... use `takeWhile` instead of `filter` because `filter` doesn't work on an infinite list
sqrtSum :: Int
sqrtSum = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
-- sqrtSum = (+ 1) . length . takeWhile (< 1000) . scanl1 (+) . map sqrt $ [1..]

--function application ($)

-- sum (map sqrt [1..130])
-- sum $ map sqrt [1..130]

-- sqrt (3 + 4 + 9)
-- sqrt $ 3 + 4 + 9

-- sum (filter (> 10) (map (*2) [2..10]))
-- sum $ filter (> 10) $ map (*2) [2..10]

-- map ($ 3) [(4+), (10*), (^2), sqrt]

--function composition (.)

-- map (\x -> negate (abs x)) [1,2,3]
-- map (negate . abs) [1,2,3]

-- map (\xs -> negate (sum (tail xs))) [[1..5], [3..6]]
-- map (negate . sum . tail) [[1..5], [3..6]]

-- sum (replicate 5 (max 6.7 8.9))
-- sum . replicate 5 . max 6.7 $ 8.9

-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
