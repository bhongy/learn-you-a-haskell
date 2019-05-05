# 09. Input and Output

## Hello, World!

- compile program (to executable) with `ghc <path-to/source-file>`
- when execute a program via terminal, `IO` with the name `main` will be run
- compile and run program in one command by `runhaskell <path-to/source-file>`
- intuition `>>=`: next computation relies on the result of the previous
- intuition `>>`: next computation ignores the result of the previous (but not its side-effect)
- `IO` separates pure/impure part of the code because calling impure (`IO`) functions like `getLine` does not guarantee to return the same result when calling it the next time
- `IO` will only be performed if it eventually falls into `main`
  - or running the `IO` action in `ghci`
  ```
  # in ghci
  > :l src/09-input-and-output.hs
  > greeting
  ```
- `return` makes an IO action from a pure value (lifting the value to IO)

## Useful IO functions

- `putStr`
- `putChar`
- `print` (like `putStrLn . show`)
- `getChar`
- `when` takes a boolean and an IO action - "run" it if the boolean is `True`
- `sequence` performs multiple IO actions and binds their results in a list
  - `sequence :: [IO a] -> IO [a]`
- `mapM` sequence map combined
- `mapM_` like `mapM` but throws away the result
- `forever` takes an IO action and repeats it forever
- `forM` 
