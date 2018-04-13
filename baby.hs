doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2 -- else is mandatory

-- listComprehension = take 10 [2,4 ..]
-- > [2,4,8,10,14,16,20,22,26,28,32,34,38,40]
listComprehension :: [Int]
listComprehension = [x * 2 | x <- [1 .. 20], x `mod` 3 /= 0]

-- boomBangs [40..50]
-- > ["BANG!","BANG!","BOOM!","BANG!","BANG!"]
-- using multiple filters it's like chaining them with `&&`
boomBangs :: Integral a => [a] -> [String]
boomBangs xs =
  [ if mod x 5 == 0
    then "BOOM!"
    else "BANG!"
  | x <- xs
  , odd x
  ]

multipleFilters :: [Int] -> [Int]
multipleFilters xs = [x | x <- xs, x /= 13, x /= 15, x /= 19]

-- > [55,80,100,110]
-- but normally just use `length xs`
twoListsComprehension :: [Int]
twoListsComprehension = [x * y | x <- [2, 5, 10], y <- [8, 10, 11], x * y > 50]

length' :: Integral a => [t] -> a
length' xs = sum [1 | _ <- xs]

-- removeNonUppercase "Hahaha! Ahahaha!"
-- > "HA"
-- removeNonUPpercase "IdontLIKEFROGS"
-- > "ILIKEFROGS"
removeNonUppercase :: String -> String
removeNonUppercase str = [c | c <- str, elem c ['A' .. 'Z']]

{--
  takeEvenNested
    [ [1, 3, 5, 2, 3, 1, 2, 4, 5]
    , [1, 2, 3, 4, 5, 6, 7, 8, 9]
    , [1, 2, 4, 2, 1, 6, 3, 1, 3, 2, 3, 6]
    ]
--}
-- > [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
takeEvenNested :: Integral a => [[a]] -> [[a]]
takeEvenNested xxs = [[x | x <- xs, even x] | xs <- xxs]

rightTriangles :: [(Int, Int, Int)]
rightTriangles =
  [ (a, b, c)
  | c <- [1 ..] -- hypothenuse
  , b <- [1 .. c] -- b is eq or smaller than c
  , a <- [1 .. b] -- a is eq or smaller than b
  , a ^ 2 + b ^ 2 == c ^ 2 -- right triangle
  -- if we want to get only the one with perimeter 24 with `take 1 rightTriangles`
  -- , a + b + c == 24
  ]
