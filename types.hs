import qualified Data.Map as Map
import Shapes

-- a value constructor is just a function so we can map it over a list
listOfCircles = map (Circle (Point 0 0)) [10, 20, 30]

-- surface $ Circle (Point 10 20) 10
-- 314.15927

-- surface $ Rectangle (Point 0 0) (Point 10 10)
-- 100.00

-- nudge (Circle (Point 34 34) 10) 5 10
{-
[ Circle (Point 0.0 0.0) 10.0
, Circle (Point 0.0 0.0) 20.0
, Circle (Point 0.0 0.0) 30.0
]
-}


-- # Records

data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  , height :: Float
  , phoneNumber :: String
  , flavor :: String
  } deriving Show

me = Person { firstName="Christopher"
            , lastName="Robin"
            , age=5
            , height=3.2
            , phoneNumber="n/a"
            , flavor="honey"
            }

-- firstName me
-- >> "Christopher"

-- # Type parameters

-- data Maybe a = Nothing | Just a {- already in Prelude -}
-- try with its value constructor `Just 1` or `Nothing`

data Car = Car
  { company :: String
  , model :: String
  , year :: Int
  } deriving Show

tellCar :: Car -> String
tellCar Car { company = c, model = m, year = y } =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- tellCar $ Car { company="Audi", model="A6", year=1995 }

data Vector a = Vector a a a deriving Show

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

-- Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
-- >> Vector 12 9 19

vmulti :: (Num t) => Vector t -> t -> Vector t
vmulti (Vector i j k) m = Vector (i*m) (j*m) (k*m)

-- Vector 3 9 7 `vmulti` 10
-- >> Vector 30 90 70

-- scalar product of two vectors
scalarMulti :: (Num t) => Vector t -> Vector t -> t
scalarMulti (Vector i j k) (Vector l m n) = i*l + j*m + k*n

-- Vector 4 9 5 `scalarMulti` Vector 9.0 2.0 4.0
-- >> 74.0

-- # Derived instances

data Product = Product
  { pid :: String -- because `id` is already a function (identity)
  , name :: String
  } deriving (Eq, Show, Read)

macbook = Product { pid="123", name="macbook" }
ipad = Product { pid="abc", name="ipad" }

-- macbook == ipad
-- >> False
-- macbook == Product { pid="123", name="macbook" }
-- >> True
-- Product { pid="abc", name="ipad" } `elem` [macbook, ipad]
-- >> True

-- need to annotate the type (`:: Product`) because haskell can't infer
-- read "Product { pid =\"abc\", name =\"ipad\" }" :: Product
-- >> Product {pid = "abc", name = "ipad"}
-- the type can be inferred from the comparison, no need `::`
-- read "Product { pid =\"abc\", name =\"ipad\" }" == macbook
-- >> False

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Saturday > Friday
-- >> True
-- Monday `compare` Wednesday
-- >> LT
-- minBound :: Day
-- >> Monday
-- maxBound :: Day
-- >> Sunday
-- succ Monday
-- >> Tuesday
-- pred Saturday
-- >> Friday

-- [Thursday .. Sunday]
-- >> [Thursday,Friday,Saturday,Sunday]

-- [minBound .. maxBound] :: [Day]
-- >> [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

-- ## Type synonyms (aliasing)

{-
  Either is already in prelude
  try with its value constructor `Left 0`

  data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-}

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing ->
      Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
      if state == Taken
      then
        Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
      else
        Right code

lockers :: LockerMap
lockers = Map.fromList
  [ (100, (Taken, "ZD39I"))
  , (101, (Free, "JAH3I"))
  , (103, (Free, "IQSA9"))
  , (105, (Free, "QOTSA"))
  , (109, (Taken, "893JJ"))
  , (110, (Taken, "99292"))
  ]

-- lockerLookup 100 lockers
-- lockerLookup 101 lockers
-- lockerLookup 102 lockers

-- # Recursive data structures

-- data List a = Empty | Cons a (List a) deriving (Eq, Ord, Show, Read)
-- `Cons` is our implementation of `:` it's a value constructor that takes an element and a list then return a list
-- Cons 4 $ Cons 5 Empty

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Eq, Ord, Show, Read)
-- 4 :-: 5 :-: Empty

infixr 5  .++
(.++) :: List a -> List a -> List a
(.++) Empty ys = ys
(.++) (x :-: xs) ys = x :-: (xs .++ ys)

-- (3 :-: 4 :-: 5 :-: Empty) .++ (6 :-: 7 :-: Empty)

-- Binary Search Tree
data Tree a
  = EmptyTree
  | Node a {- value -} (Tree a) (Tree a)
  deriving (Eq, Show, Read)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- inserting to a tree creates a new tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
-- if an empty sub-tree, we're where we want
-- and instead of the empty tree, we put a singleton tree with our element
treeInsert x EmptyTree = singleton x
-- otherwise, more "traversal" to do
treeInsert x (Node a left right)
  -- return as-is
  | x == a = Node x left right
  -- ask left subtree to insert x
  -- another way to look at it is to return the node with everything the same
  -- except the new left subtree with x inserted to it
  | x < a = Node a (treeInsert x left) right
  -- ask right subtree to insert x
  | x > a = Node a left (treeInsert x right) 

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

numsTree = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]

-- 8 `treeElem` numsTree
-- >> True
-- 100 `treeElem` numsTree
-- >> False

-- # Typeclass 102

{-
  -- defining a typeclass
  class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    -- these are not necessary but implemented so that
    -- we can only need to override either `==` or `/=` of the instance
    x == y = not x /= y
    x /= y = not x == y
-}

-- didn't derive instance we will define manually
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

-- Red == Red
-- Red == Yellow
-- Red `elem` [Red, Yellow, Green]

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- for a type constructor (subclassing a typeclass)
-- notice type constrain `Eq a` so that we can compare the contained values
{-
instance (Eq a) => Eq (Maybe a) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False
-}

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

{-
 -- not (Maybe a) because Functor wants a type constructor not a concrete type
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing
-}

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftTree rightTree) = Node (f x) (fmap f leftTree) (fmap f rightTree)

{-
 -- if type constructor takes two type parameters, partially apply it
  instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

  fmap :: (b -> c) -> Either a b -> Either a c
-}

