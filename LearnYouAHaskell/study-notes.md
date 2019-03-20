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

## Algebraic data types

```haskell
-- data <TypeName> = ValueConstructor P1 P2 | ...
-- example
data Bool = True | False
```

- use `data` keyword to define a new type:
- type name has to be capitalized
- value constructors can be declared inside the type declaration
- we only define function types against types not value constructors
- value constructors can take type parameters
- value constructor is _just a function_ that returns a value of the data type
- use type (e.g. `Shape`) not value constructor in function type declaration
- use value constructors (e.g. `Circle`, `[]`) to pattern match in function implementation
- you can use value contructor to construct values outside type declaration (e.g. `Maybe 10`)
- it's common to use the same name as the type if there's only one value constructor
- to export all value constructors of the data type use `(..)` like `Shape(..)`
- to export specific value constructors of a type use `Shape(Circle, Rectangle)`
- to export only the data type without the value constructors use `Shape`
  - then export auxillary functions like `Map.fromList` instead
  - users cannot pattern match againsts the value constructor
  - it's a design pattern to hide implementation detail (there're appropriate usages for either design)

## Record

- example: `data Person = Person { firstName :: String, lastName :: String }`
  - use `Person { firstName="john", lastName="doe" }`
- accessor functions will be automatically created like `firstName`

## Type parameters

- we generally call the type "type constructor" if it takes one or more type parameters (it produces a new type)
- example: `data Maybe a = Nothing | Just a`
- as I understand, it's the same as generic.
- **never add typeclass constraints in `data` declarations**
  - like never do: `data (Ord k) => Map k v = ...` because it forces functions that doesn't care about that constrain to have to include that constrain too

## Derived instances

- use `deriving` keyword
- for `Ord` typeclass, the value constructor that's defined first considered smaller
  - e.g. `data Bool = False | True deriving (Ord)` - `False` is considered smaller
- Haskell can automatically make type instance for deriving from `Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read`

## Type synonyms (aliasing)

- use `type` keyword
- example: `type String = [Char]`
- just give a same type different names (don't really do anything)
- mostly for documentation
- it can be parameterized `type AssocList k v = [(k,v)]`
- we can partially apply type parameters of a generic (type constructor) to get a new generic

## Recursive data structures

- `infixr 5 :-:` declare an infix operator (infix "right" priority 5 to `:-:`)
  - to make infix operator (without using \`\`) the function name has to contains only special characters
- pattern matching is actually about matching constructures
- `Data.Set` and `Data.Map` are implemented with balanced binary search tree internally

## Typeclasses 102

Making your own typeclasses and manually derive from typeclasses.

- typeclass is like an interface
- when we say that a type is an instance of a typeclass, we mean that we can use the functions that the typeclass defines with that type
- `Read` is to parse string to a type
- typeclasses have nothing to do with classes in OO languages
- definining a new typeclass
  - use `class ... where`
  - not necessary to implement body of the functions
  - only type definition of the functions are required
- use `instance <Typeclass> <Concrete Class> where` to create an instance of the typeclass without deriving from it
- subclassing a typeclass: `class (Eq a) => Num a where`
  - basically just put class constrain on a class declaration
- `ghci> :info Maybe` to see the instances of a typeclass

## Tokens/functions reference
https://www.haskell.org/hoogle/

`div` returns Integral division of two numbers while `/` returns Fractional
`==` equal
`/=` not equal
`!!` index accessing a list element: `[1,5,10] !! 1` (returns 5)
`$` function application (right-associative) (function application with space is left-associative)

## Other notes

> in functional programming, TDD is not really needed to "drive the design", because the language itself does the same job, but in a slightly different way:  Good design is the path of least resistance in Haskell, and since re-factoring is so safe and easy, it almost automatically tends in that direction.

> Haskell strictly separates evaluation order from side effect order.
http://www.haskellforall.com/2015/03/algebraic-side-effects.html

## Thought about things I want to see better in Haskell (maybe)

- available functions like `head`, `tail` and `id` prevent using those fields on the record (because it'll create the same function that crashes with those names)
- also records are very loud in the scope (due to the creation of accessor functions) which means you cannot create multiple record data types using the same fields
