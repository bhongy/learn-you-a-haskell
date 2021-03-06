# 02. Starting Out

## Functions & if-else expression

- Function application (calling a function by putting a space after it and then typing out the parameters) has the highest precedence of them all.
- Functions in Haskell don't have to be defined in a particular order so you can define a function that uses another function that is declared later.
- `if`, `else` in haskell is an expression - must return a value
- `else` is mandatory
- functions can't begin with uppercase letters.

## List & String

strings are lists and we can use list functions on them

Watch out when using `++` for large original (left-side) list (or string) because haskell has to walk through the whole left-side list to append (linear time). Using `:` (prepend - cons) is constant time.

```haskell
'A':" SMALL CAT"  
-- "A SMALL CAT"  
5:[1,2,3,4,5]  
-- [5,1,2,3,4,5]  
```

Lists can be compared if the stuff they contain can be compared - they are compared in lexicographical order. << good to avoid writing loops to compare list elements

```haskell
[3,4,2] > [2,4]  
-- True  
[3,4,2] == [3,4,2]  
-- True  
```

Useful list functions

- `head :: [a] -> a`
- `tail :: [a] -> [a]` (always return a list: `tail [1] -> []`)
- `last :: [a] -> a`
- `init :: [a] -> [a]` (opposite of `last`)
^ **all these methods will throw an exception if called on an empty list**

- `length :: [a] -> Int` 
- `null :: [a] -> Bool` - returns `True` if the list is empty use this instead of `xs == []`
- `reverse :: [a] -> [a]`
- `take :: Int -> [a] -> [a]` - takes a number of elements from the beginning of the list
- `drop :: Int -> [a] -> [a]` - drops a number of elements from the beginning of the list
- `sum`
- `elem :: a -> [a] -> Bool` - check if the value is in the list

Ranges -> `[2,4..20]` or `['a'...'z']` or `[20,19..1]` or `[13,26..24*13]` can be used to create a range - list of sequence

*Don't use float for range.*
http://learnyouahaskell.com/starting-out

Infinite list: [13,26,..]
- `cycle [1,2,3]` (infinite list of 1,2,3,1,2,..)
- `repeat`
- `replicate`

List comprehension

```haskell
-- [ <expression> | <binding> <- <seed>, <where> ]
[x*2 | x <- [1..10], x*2 >= 12]

-- multiple filters
[x | x <- [10..20], x /= 13, x /= 15, x /= 19]

-- multiple seeds
[x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
```

## Tuples

- `("name", 1, True)`
- 2-tuples (pair) / 3-tuples (triple)
- safer to model something like known size list of items (e.g. vector)
- useful functions for pairs `fst` and `snd` (only pairs)
- `zip :: [a] -> [b] -> [(a, b)]` - create list of pairs from two lists
  ^ good for traversing two lists simultaneously
  - if two lists are different lengths - it only goes to the shorter one
  - you can zip an infinite list to a finite list (or to another infinite list)
