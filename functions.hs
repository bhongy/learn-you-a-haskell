lucky :: Integral a => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal."

_factorial :: Integral a => a -> a
_factorial n = product [1 .. n]

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

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
