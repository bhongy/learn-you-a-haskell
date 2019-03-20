------------
-- Recursion
-- http://learnyouahaskell.com/recursion
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
