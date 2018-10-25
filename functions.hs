lucky :: Integral a => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal."

_factorial :: Integral a => a -> a
_factorial n = product [1 .. n]

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- tail-recursive factorial
factorial' :: Integral a => a -> a
factorial' n = _factorial 1 n
  where
    _factorial result 0 = result
    _factorial result n = _factorial (n * result) (n - 1) 

addVectors :: Num a => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a, snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- pattern matching for tuple
third :: (a, b, c) -> c
third (_, _, z) = z

-- pattern matching for list
head' :: [a] -> a
head' []    = error "Cannot operate on empty list"
head' (x:_) = x

tell :: Show a => [a] -> String
tell []       = "empty"
tell (x:[])   = "one element: " ++ show x
tell (x:y:[]) = "two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_)  = "long list - starts with: " ++ show x ++ " and " ++ show y

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

{--

tailRecursiveLength :: (Integral n) => [a] -> n
tailRecursiveLength xs = _length 0 xs
  where
    _length result [] = result 
    _length result (_:xs') = _length (result + 1) xs'

tailRecursiveSum :: (Num a) => [a] -> a
tailRecursiveSum xs = _sum 0 xs
  where
    _sum result [] = result
    _sum result (x:xs') = _sum (result + x) xs'

--}

-- using "pattern" `@`
capital :: String -> String
capital ""         = "No empty string. Stop it."
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: RealFloat a => a -> a -> String
bmiTell weight height
  -- a guard is a boolean expressions
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Normal"
  | bmi <= chubby = "Overweight"
  | otherwise = "Obese"
    -- these names are visible across guards
    -- but are not shared across patterns
  where
    bmi = weight / height ^ 2
    (skinny, normal, chubby) = (18.5, 25.0, 30.0)

max' :: Ord a => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: Ord a => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

initials :: String -> String -> String
-- initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f:_) = firstname
    (l:_) = lastname

cylinderSurface :: RealFloat a => a -> a -> a
cylinderSurface r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

-- case pattern matching
describeList :: [a] -> String
describeList xs =
  "The list is " ++
  case xs of
    []  -> "empty."
    [x] -> "a singleton list."
    _   -> "a long list."



------------
-- Recursion
------------

maximum' :: (Ord a) => [a] -> a
maximum' []     = error "maximum of empty list"
maximum' [x]    = x
-- maximum' (x:xs) = max x (maximum' xs)
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
  -- don't use `otherwise` here to fall through to the next pattern
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
-- the sorted list is a list that has all values smaller than the head in front
-- then the head and all values larger than the head in back
quicksort (x:xs) =
  let front = quicksort [a | a <- xs, a <= x]
      back = quicksort [a | a <- xs, a > x]
  in front ++ (x : back)



-------------------
-- Higher-order Functions
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

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
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
sum'' :: (Num a) => [a] -> a
-- sum'' xs = foldl (\acc x -> acc + x) 0 xs
sum'' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x = foldl (\acc y -> if x == y then True else acc) False

map'' :: (a -> b) -> [a] -> [b]
-- use `:` is much more efficient than `++`, hence foldr
map'' f = foldr (\x acc -> f x : acc) []

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []
-- reverse '' = foldl (flip (:)) []

product'' :: (Num a) => [a] -> a
product'' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []
 
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
