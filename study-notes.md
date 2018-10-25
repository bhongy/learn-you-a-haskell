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
