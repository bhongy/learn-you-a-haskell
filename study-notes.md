## Functions & if-else expression

- Function application (calling a function by putting a space after it and then typing out the parameters) has the highest precedence of them all.
- Functions in Haskell don't have to be in any particular order, so it doesn't matter if you define doubleMe first and then doubleUs or if you do it the other way around.

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

Ranges -> [2,4..20] or ['a'...'z'] or [20,19..1] or [13,26..24*13] can be used to create a range - list of sequence
Don't use float for range.

Infinite list: [13,26,..]
- `cycle [1,2,3]` (infinite list of 1,2,3,1,2,..)
- `repeat`
- `replicate`

## Tuples

- `("name", 1, True)`
- 2-tuples (pair) / 3-tuples (triple)
- safer to model something like known size list of items (e.g. vector)
- useful functions for pairs `fst` and `snd` (only pairs)
- `zip :: [a] -> [b] -> [(a, b)]` - create list of pairs from two lists
  ^ good for traversing two lists simultaneously
  - if two lists are different lengths - it only goes to the shorter one
  - you can zip an infinite list to a finite list (or to another infinite list)

## Types

- Good practice: giving explicit type declaration when writing a function
- `Int` is 32-bit integer [-2147483648 .. 2147483647]
- `Integer` can be used for larger numbers than `Int`
- `Float` is single-precision floating point number
- `Double` is double-precision floating point number
- `Bool`, `Char`
- `()` is empty tuple
- Functions that have type variables are called *polymorphic functions*
- typing function for tuple: `fst :: (a, b) -> a`

## Typeclasses

- `(Ord a, Num a) => a -> a` : `=>` calls class constraint
- `Eq` is for types that support equality testing. Implements `==` and `/=`
- All standard Haskell types except for IO and functions are a part of the `Eq` typeclass
- `Ord` is for types that support ordering. `<` `>` `<=` `=>` `compare`
- `Show` is for types that can be represented as string. Implements `show`
- `Read` is for types that can be converted from string. Implements `read`
  - `read "5" :: Int` or `read "5" + 5`
  - ^ Haskell is statically typed so it has to know all types at compile time
- `Enum` is for types that are sequentially ordered (can be enumerated).
  - can be used in list ranges
  - implements `pred` (predecessor) and `succ` (successor)
  - `succ 'B' -- > 'C'`
  - Types in this class: (), Bool, Char, Ordering, Int, Integer, Float and Double.
- `Bounded` is for types with upperbound & lowerbound (Int, Char, Bool)
  - tuples can be part of `Bounded` if all of their components belong in `Bounded`
- `Num` - numeric
- `Integral` - whole numbers (`Int`, `Integer`)
  - `fromIntegral` can be used to convert from Integral to Num
- `Floating` - `Float`, `Double`

## Other notes

> in functional programming, TDD is not really needed to "drive the design", because the language itself does the same job, but in a slightly different way:  Good design is the path of least resistance in Haskell, and since re-factoring is so safe and easy, it almost automatically tends in that direction.
