# 03. Types and Tyepclasses

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

## Typeclasses 101

- compare to OO classes typeclasses are more like interfaces
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
