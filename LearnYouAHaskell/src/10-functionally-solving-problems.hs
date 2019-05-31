-------------------
-- 10. Functionally Solving Problems
-- http://learnyouahaskell.com/functionally-solving-problems
-------------------

-- Reverse Polish notation calculator

solveRPN :: String -> Float
solveRPN = head . foldl eval [] . words
  where
    eval (x:y:rest) "+"  = (y + x):rest
    eval (x:y:rest) "-"  = (y - x):rest
    eval (x:y:rest) "*"  = (y * x):rest
    eval (x:y:rest) "/"  = (y / x):rest
    eval (x:y:rest) "^"  = (y ** x):rest
    eval (x:xs) "ln"     = log x : xs
    eval xs "sum"        = [sum xs]
    eval xs numberString = read numberString : xs

-- Heathrow to London

type Cost = Int
-- data Node = Node Road Road | EndNode
-- data Road = Road Cost Node
data Section = Section { a :: Cost, b :: Cost, c :: Cost } deriving Show
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon =
  -- or [ Section { a = 50, b = 10, c = 30 }, ... ]
  [ Section 50 10 30
  , Section 5 90 20
  , Section 40 2 25
  , Section 10 8 0
  ]

data Label = A | B | C deriving Show
type Path = [(Label, Cost)]

-- optimalPath heathrowToLondon -> [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
    then reverse bestAPath
    else reverse bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let aSoFar = sum $ map snd pathA
      bSoFar = sum $ map snd pathB
      aToA = aSoFar + a
      bToA = bSoFar + b + c
      bToB = bSoFar + b
      aToB = aSoFar + a + c
      newPathToA = if aToA <= bToA then (A, a):pathA else (C, c):(B, b):pathB
      newPathToB = if bToB <= aToB then (B, b):pathB else (C, c):(A, a):pathA
  in (newPathToA, newPathToB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

-- run in terminal: `cat paths.txt | runhaskell <thisfile>.hs`
main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a,b,c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concatMap (show . fst) path
      pathCost = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The cost is: " ++ show pathCost
