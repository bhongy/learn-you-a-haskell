# 08. Making Our Own Types and Typeclasses

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
