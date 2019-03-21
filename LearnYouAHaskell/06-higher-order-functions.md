# 06. Higher order functions

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
- an intuition is: `a $ [expression]`

## Function Composition (.)

- is normally used to compose functions on the fly and pass it to a higher-order function - normally more concise than using lambdas if you don't have to massage inputs
- to rewrite an expression with a lot of parentheses: starts with `$` to the last parameter of the inner most function application then convert the rest using `.`  - if the expression ends with 3 parentheses, chances are that you'll end up with 3 `.`
- useful to write point-free style functions like `fn = ceiling . abs . max`
- avoid writing long chain of function composition but use `let` bindings to give labels to the intermediate function bindings (sub-problems) instead
