-------------------
-- 09. Input and Output (02. Files and Streams)
-- http://learnyouahaskell.com/input-and-output
-------------------

{- To test all these functions. Just run them in ghci -}

import Data.Char
import Data.List (delete, (!!))
import System.IO
import System.Directory

-- usage - in command line: `cat file.txt | ./capslocker`
capslocker = do
  contents <- getContents
  putStr (map toUpper contents)

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter ((< 10) . length) . lines

{-
ioShortLines = do
  contents <- getContents
  putStr (shortLinesOnly contents)
-}

ioShortLines = interact shortLinesOnly
-- ioShortLines = interact $ unlines . filter ((< 10) .length) . lines

respondPalidromes :: String -> String
respondPalidromes = unlines . map respond . lines
  where
    isPalindrome xs = xs == reverse xs
    respond xs = if isPalindrome xs then "palindrome" else "not palindrome"

-- openFile :: FilePath -> IOMode -> IO Handle
catFileContent :: String -> IO ()
catFileContent filename = do
  handle <- openFile filename ReadMode
  -- hGetContents knows how to "read" (stream) the content of file Handle
  contents <- hGetContents handle
  putStr contents
  -- need manual clean up
  hClose handle

-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
catFileContent2 :: String -> IO ()
catFileContent2 filename = do
  -- withFile will handle closing the file when the handler returns
  withFile filename ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result

{-
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)
-}

addTodo = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")

removeTodo = do
  srcHandle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents srcHandle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStrLn "These are your TO-DO items:"
  -- remember `unlines` "join" list of strings with "\n"
  -- putStr $ unlines numberedTasks
  mapM putStrLn numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  -- ghc can infer type of number in the next line
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose srcHandle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
