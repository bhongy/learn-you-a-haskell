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

## Types

- Good practice: giving explicit type declaration when writing a function
- `Int` is 32-bit integer [-2147483648 .. 2147483647]
- `Integer` can be used for larger numbers than `Int`
- `Float` is single-precision floating point number
- `Double` is double-precision floating point number
- `Bool`, `Char`
- `()` is empty tuple
- Functions that have type variables are called *polymorphic functions* (e.g. `head :: [a] -> a`)
- typing function for tuple: `fst :: (a, b) -> a`

## Typeclasses

- `(Ord a, Num a) => a -> a` : `=>` calls class constraint
- `Eq` is for types that support equality testing. Implements `==` and `/=`
- All standard Haskell types except for IO and functions are a part of the `Eq` typeclass
- `Ord` is for types that support ordering. `<` `>` `<=` `=>`
  - `compare :: Ord a => a -> a -> Ordering`
  - `Ordering = GT | LT | EQ`
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
  - `fromIntegral` can be used to convert from Integral to Num - use when you want integral and floating point types to work together
- `Floating` - `Float`, `Double`

## Pattern Matching

- pattern will be checked from top to bottom
- non-exhaustive pattern matching will compile but is unsafe (can fail)
- `x:xs` only match against lists of length 1 or more (need `[]` base case)
- `x:y:z:rest` will binds the first three elements of the list to x,y,z
- need to wraps pattern matching in paren `length' (_:xs) = 1 + length' xs`
- `xs@(x:y:ys)` will bind the whole list to `xs`

## Guards

- whereas pattern matching ensure the form of values, guards are to test whether some property of values are true or false
  - patterns: structure/form of input
  - guards: test the value of input itself (like if statements)
- it's pretty similar to case statements (or if/else) in imperative languages
- if the value falls through all guards (without `otherwise`) then the next pattern is evaluated
- ** remember there's no `=` between function name (declaration) and the first guard (common syntax error by newbies)
- `where` is good to make the guards more readable and avoid repeating calculations
- `where` can be nested
- `let <bindings> in <expression>`
- `let` bindings don't span across guards only in the expression after `in` part
- `let` can be used to introduce function in local scope
  - `[let square x = x * x in (square 5, square 3, square 2)]`
- use `;` to separate `let` bindings if you can't break it to multiple columns
  - `(let a = 100; b = 200; c = 300 in a*b*c)`
- `let` can be used for pattern matching
- `let` can be used in list comprehension
  - it'll be available tothe output function (the part before `|`) as well as all predicates
  - we can omit `in` when using `let` in list comprehension
  - `let` is an expression
- `case` can be used similar to function pattern matching but it allows more fine-grained usage - they are useful for pattern matching against something in the middle of an expression

## Recursion

- Recursion is important to Haskell because unlike imperative languages, you do computations in Haskell by declaring what something *is* instead of declaring *how* you get it.
- the edge case often is the identity (empty list, 0 - addition, 1 - multiplication)
- trees: the edge case is usually a node that doesn't have any children.

## Higher-order Functions

- higher-order functions are functions that either (or both)
  1. take functions as a parameter 
  2. return functions as return values
- it's core to haskell: define computations by defining what stuff is (declarative) rather than using an imperative approach
- infix functions can also be partially applied to either first or second param
  - first param: `powerOf2 = (2 **)` or `powerOf2 = (**) 2`
  - second param: `divideByTen = (/ 10)` (not the same as `(/) 10` < that is `(10 /)`)
- you can't do `(- 4)` to create `subtract4` because it'll create negative 4 instead of a partially applied function. Instead, do `subtract4 = subtract 4`.
- `foldl` fold from the left side (starts from the head of the list)
- `foldr` callback signature is `\x acc -> ...` when `foldl` callback signature flips the opposite `\acc x -> ...` --- think the accumulator is the previous position of the item being fold or think the "sweeping end" of the folding is `acc`
- `x:xs` is much more efficient than `xs ++ x` so we generally use `foldr` when buliding up a list from a lits
- `foldr` works on infinite lists while `foldl` doesn't.

> **Folds can be used to implement any function where you traverse a list once, element by element, and then return something based on that.** Whenever you want to traverse a list to return something, chances are you want a fold.

- `foldl1` and `foldr1` can be used without supplying the initial value but they will throw runtime exceptions if calling on an empty list --- this can be a good behavior for building functions that calling it on an empty list doesn't make sense (e.g. maximum)
- `scanl` and `scanr` is similar to `foldl` and `foldr` but will report the intermediate result into the list (where fold returns the end result at the end). 
- scans are used to monitor the **progression** of the fold such as:
  - _How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?_

## Lambda Expressions

- `(\a b -> (a * 30 + 3) / b)`
- usually wraps in parentheses because it extends all the way to the end of the line
- they are expressions (always return a value)

## Function Application ($)

- have the lowest precedence ("space" function application has the highest precedence)
- right-associative ("space" function application is left-associative)
- When a `$` is encountered, the expression on its right is applied as the parameter to the function on its left.
- You can imagine a $ being sort of the equivalent of writing an opening parentheses and then writing a closing one on the far right side of the expression.

## Function Composition (.)

- is normally used to compose functions on the fly and pass it to a higher-order function - normally more concise than using lambdas if you don't have to massage inputs
- to rewrite an expression with a lot of parentheses: starts with `$` to the last parameter of the inner most function application then convert the rest using `.`  - if the expression ends with 3 parentheses, chances are that you'll end up with 3 `.`
- useful to write point-free style functions like `fn = ceiling . abs . max`
- avoid writing long chain of function composition but use `let` bindings to give labels to the intermediate function bindings (sub-problems) instead

## Modules

- `import` must be done before defining any functions - should be done at the top of the file.
- `import <module>` will make all functions in the module becomes available in global namespace.
- `import <module> (fun1, fun2)` to import only the selected functions from the module
- `import <module> hiding (fun1)` to "not" import certain functions from the module
- `import qualified <module>` using the function has to be qualified like `Data.Map.filter` rather than `filter`
- `import qualified <module> as <alias>` can be used to alias the qualified name like `M.filter`
- [List of standard libraries](https://downloads.haskell.org/~ghc/latest/docs/html/licbraries/) (e.g. Data.List)

## Module: Data.List

- `intersperse` add element between each items in the list. Can use to build string like: `intersperse "/" ["foo", "bar", "18f03cc732"]`
- `transpose` useful to do a reduce per column like adding polynomials: `map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]`
- `foldl'` and `foldl1'` use when encounter stack overflow due to `foldl` lazy evaluation
- `concat` is basically flatten
- `concatMap` is map then concat
- `and`, `or`, `any`, `all`
- `iterate` create infinite list (applies iterating function to the result of previous iteration infinitely) - e.g. `iterate (*2) 1` produces `[1,2,4,8,..]`
- `splitAt` split list into a pair (tuple) at an index - pattern matching to destructure the tuple is helpful
- `takeWhile` return part of the list up until the first time the predicate returns false - good to use with infinite lists
- `dropWhile` drops part of the list until the fist time the predicate is true - then returns the rest of the list
  - can use `head . dropWhile _predicate_` to achieve something like "find"
- `span` is similar to `takeWhile` but returning a pair tuple whose first part is the part that'd return from `takeWhile` and the second part is the dropped part of the list.
- `break` is like the negative version of `span` - doing `break p` is equivalent to `span (not . p)`
- `sort`
- `group` groups _adjacent_ elements into sublists if they're equal
- `inits`, `tails`, `isInfixOf`, `isPrefixOf`, `isSuffixOf`
- `elem`, `notElem` checks if the list includes the element
- `partition` takes a list and a predicate and return a pair of lists
  - similar to `span` and `break` but put _all_ elements passing the predicate to one list and ones failing predicate to another list while `span` and `break` stops the first time the predicate fails
- `find` returns the _first_ element that satisfies the predicate wraps in `Maybe`
  - `Maybe` can be either `Just <element>` (one element) or `Nothing`
- `elemIndex`, `elemIndices`
- `findIndex`, `findIndices`
- `lines` takes a string and break it at line breaks into list of strings
- `unline` joins a list of string to one string with line breaks
- `words`, `unwords` like `lines`, `unlines` but for words
- `nub` dedupe elements
- `delete` delete the first `<element>` appears in a list
- `\\` removes elements in a second list from the first list (think subtract)
- `union` goes over every element in the 2nd list and appends it to the first list if the element isn't already in it. Duplicates are removed from the 2nd list.
- `intersect`
- `insert` starts at the beginning of the list and insert the element right before the first element it encounters that is equal to or larger than the element provided.
- use these generic version if you need `Integral` output rather than `Int`: `genericLength`, `genericTake`, `genericDrop`, `genericSplitAt`, `genericIndex`, `genericReplicate`
- `nubBy`, `deleteBy`, `unionBy`, `intersectBy`, `groupBy`: generic versions that takes a function that checks for equality
  - when dealing with these type of "By" functions we generally do `on (==) <something>`
- `sortBy`, `insertBy`, `maximumBy`, `minimumBy`: generic versions that takes a function that compares greater/smaller quality
  - `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`
  - when dealing with these type of "By" functions we generally do `on compare <something>`

## Data.Function

- `on :: (b -> b -> c) -> (a -> b) -> a -> a -> c` map `a` of the two inputs to `b` then combines `b` results to create `c`. Examples:

```haskell
sortBy (compare `on` fst) values
groupBy ((==) `on` (> 0)) values
# 
```

## Data.Char

- `isSpace`, `isLower`, `isUpper`, `isAlpha`, `isAlphaNum`, `isDigit`, `isHexDigit`, `isNumber`, `isPunctuation`, `isSymbol`, `isSeparator`, `isAscii`, `isAsciiUpper`, `isAsciiLower`
- `generalCategory <char>` is used to determine the `GeneralCategory` type of a specific character (e.g. `Space`, `DecimalNumber`, `UppercaseLetter`, `OtherPunctuation`)
  - `GeneralCategory` is part of `Eq` typeclass so you can do something like `generalCategory ' ' == Space`
- `toUpper` (noop for space, numbers, etc), `toLower`, `digitToInt` (hex), `intToDigit` (hex)
- `ord` converts character to the corresponding character code (`Int`) - e.g. `ord 'a' == 97`
- `char` the invert of `ord` 

## Data.Map

> Association lists (also called dictionaries) are _lists_ that are used to store key-value pairs where _ordering doesn't matter_.

- much faster association list than Data.List (internally implemented as keys)
- duplicate keys are discarded by default (unless create with `fromListWith`)
  - keep largest: `Map.fromListWith max [(k,v), ...]`
  - combine values of the same key with (+): `Map.fromListWith (+) [(k,v), ...]`
- `insert` add new key, value pair to the map
  - duplicate keys are discarded by default
  - `Map.insertWith` take a function to determine what to do when encounter duplicate keys similar to `Map.fromListWith`
- `empty` create an empty map
- `null` check if a map is empty
- `size`, `delete`
- `lookup` take a key return a value
- `singleton` create a map with exactly on mapping
- `member` check if a key is in the map or not
- `map`, `filter` operate on the value
- `keys` return a list of keys
- `elems` return a list of values
- `toList` the inverse of `fromList`

## Data.Set

- all elements in set are unique
- checking membership, inserting, deleteing are fast--internally implemented with tree like `Data.Map`
- **values are ordered**
- can be used to remove duplicates in _large_ list (faster than `List.nub`) by converting `fromList` to Set then back `toList`
  - note that the result will be _ordered_ if you need to preserve original ordering use `List.nub`
- `intersection`, `difference`, `union`
- same methods as `Data.Map`: `null`, `size`, `member`, `empty`, `singleton`, `insert`, `delete`, `map`, `filter`

## Tokens/functions reference
https://www.haskell.org/hoogle/

`div` returns Integral division of two numbers while `/` returns Fractional
`==` equal
`/=` not equal
`!!` index accessing a list element: `[1,5,10] !! 1` (returns 5)

## Other notes

> in functional programming, TDD is not really needed to "drive the design", because the language itself does the same job, but in a slightly different way:  Good design is the path of least resistance in Haskell, and since re-factoring is so safe and easy, it almost automatically tends in that direction.

> Haskell strictly separates evaluation order from side effect order.
http://www.haskellforall.com/2015/03/algebraic-side-effects.html
