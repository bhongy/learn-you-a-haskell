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

## Files and streams

- `getContents` reads from stdin until encounters an EOF character (Ctrl-D)
  - it's lazy like everything else in Haskell
  - it won't read the entire content but rather process it like stream
  - so you can use unix `cat` to pipe text to the program
  - e.g. `cat haiku.txt | ./capslocker`
- `interact` the pattern of taking IO input, transform, then output (IO)
  - takes the transform function of type `String -> String`
- reading/writing from stdin/stdout (terminal) is similar to r/w to a special type of file.
- `openFile` takes `FilePath` and `IOMode` returning `IO Handle` (file handler)
  - `FilePath` is just an alias for `String`
  - `data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode`
  - think of `Handle` as the reference to the file
- `withFile` is similar to `openFile` but will close the file automatically once the `IO` action "returns"
- `hGetLine`, `hPutStr`, `hPutStrLn`, `hGetChar` all takes a handler and work like the non-"h" counterparts
  - the `h` versions (e.g. `hGetContents`) takes file `handle`
  - `hGetContents :: Handle -> IO String`
  - `getContents :: IO String`
  - think of the "default" `putStr` has stdout handle already applied - i.e. `putStr == hPutStr stdout`

> when seeing an IO action think "will" (promise) - an instruction (data) that something will happens, hence you can compose them together - OR think of it like connecting pipes of data streams
> example: `putStr` "will" write the content (streamed) to stdout / `getLine` "will" read a line from stdin

- common file handling helpers: `readFile`, `writeFile`, `appendFile`
  - similir to `withFile` the runtime will handle the closing of the file automatically
- for text files, the default buffering is usually line-buffering
- for binary files, the default buffering is usually block-buffering (chunk, determine by the operating system)
- `hSetBuffering handle` can be set to override the default buffering
  - `NoBuffering` means it will read one character at a time (bad idea, spam disk access)

## Command line arguments

> run `stack ghc <program.hs>` to compile the program
> run `stack runhaskell <program.hs>` to compile and run the program

- use `getArgs` (`System.Environment`) to get command line arguments (strings)
- use `getProgName` to get program name







