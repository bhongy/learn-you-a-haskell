-------------------
-- 09. Input and Output (03. Command line arguments)
-- http://learnyouahaskell.com/input-and-output
-------------------

import           Data.List
import           System.Directory
import           System.Environment (getArgs, getProgName)
import           System.IO

-- todo add todo.txt "Find the magic sword of power"
-- todo view todo.txt
-- todo remove todo.txt 2

type Command = String
type Args = [String]
type Action = Args -> IO ()

dispatch :: [(Command, Action)]
dispatch =
  -- association list (key-value pair)
  [ ("add", add)
  , ("view", view)
  , ("remove", remove)
  ]

add :: Action
add [filename, todoItem] = appendFile filename (todoItem ++ "\n")

view :: Action
view [filename] = do
  contents <- readFile filename
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks

remove :: Action
remove [filename, numberString] = do
  srcHandle <- openFile filename ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents srcHandle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose srcHandle
  hClose tempHandle
  removeFile filename
  renameFile tempName filename

main = do
  (command:args) <- getArgs
  -- lookup iterate through the list (linear time)
  case lookup command dispatch of
    Nothing -> putStrLn $ "command \"" ++ command ++ "\" is not supported."
    Just action -> action args
