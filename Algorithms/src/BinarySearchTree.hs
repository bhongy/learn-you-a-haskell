-- heavily inspired by: https://gist.github.com/Kedrigern/1239141/5ee8f5f45facdf4f48785fc92a78ad4104f16537

import           Data.List (foldl)

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

empty :: Tree a -> Bool
empty Nil = True
empty _   = False

contains :: (Ord a) => Tree a -> a -> Bool
contains Nil _ = False
contains (Node v left right) x
  | x == v = True
  | x < v  = contains left x
  | otherwise = contains right x

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node x Nil Nil
insert node@(Node v left right) x
  | x == v = node
  | x < v  = Node v (insert left x) right
  | otherwise = Node v left (insert right x)

-- delete == return a new tree with the value removed
delete :: (Ord a) => Tree a -> a -> Tree a
delete Nil _ = Nil
delete node@(Node v left right) x
  | x == v = deleteRoot node
  | x < v  = Node v (delete left x) right
  | otherwise = Node v left (delete right x)

-- deleteRoot: delete a node -> replace by one of its children
deleteRoot :: (Ord a) => Tree a -> Tree a
deleteRoot Nil = Nil
-- this will also handle (Node _ Nil Nil) = Nil
deleteRoot (Node _ Nil right) = right
deleteRoot (Node _ left Nil)  = left
deleteRoot (Node _ left right) = case smallest right of
  Just v' -> Node v' left $ delete right v'
  -- means `left` is `Nil` -> similar to:
  --   deleteRoot (Node _ Nil right) = right
  -- hence, should never hit
  Nothing -> right

-- "minimum" name clashes with Prelude.minimum
smallest :: (Ord a) => Tree a -> Maybe a
smallest Nil             = Nothing
smallest (Node v Nil _)  = Just v
smallest (Node _ left _) = smallest left

largest :: (Ord a) => Tree a -> Maybe a
largest Nil              = Nothing
largest (Node v _ Nil)   = Just v
largest (Node _ _ right) = largest right

inorder :: (Ord a) => Tree a -> [a]
inorder Nil                 = []
-- TODO: replace `++` with `:`
inorder (Node v left right) = inorder left ++ [v] ++ inorder right

preorder :: (Ord a) => Tree a -> [a]
preorder Nil = []
preorder (Node v left right) = v : (preorder left ++ preorder right)

postorder :: (Ord a) => Tree a -> [a]
postorder Nil = []
postorder (Node v left right) = postorder left ++ postorder right ++ [v]

-- naive, will not balance the tree
fromList :: (Ord a) => [a] -> Tree a
fromList = foldl insert Nil
