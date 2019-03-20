# 04. Syntax in Functions

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
  - we can omit `in` when using `let` in list comprehension which will make the bindings available anywhere in the list comprehension except in the "seed" expressions
  - using `let` with `in` will make the bindings in `let` visible only inside the let-in expression (while using without `in` will make the bindings visible outside)
  - you can always use `let` instead of `where` but not the other way around - the choice is readability
  - `let` is an expression
- use `let` only when required - most of the time do `x = value` to create bindings
  - [the difference between using and not using let](https://stackoverflow.com/a/42403968/3216606)
- `case` can be used similar to function pattern matching but it allows more fine-grained usage
  - they are useful for pattern matching against something in the middle of an expression
