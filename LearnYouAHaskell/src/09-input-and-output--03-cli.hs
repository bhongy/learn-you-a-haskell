-------------------
-- 09. Input and Output (03. Command line arguments)
-- http://learnyouahaskell.com/input-and-output
-------------------

{- To test all these functions. Just run them in ghci -}

import System.Environment (getArgs, getProgName)
-- import System.Directory
-- import System.IO
-- import Data.List

-- todo add todo.txt "Find the magic sword of power"
-- todo view todo.txt
-- todo remove todo.txt 2

type Command = String
type Args = [String]

dispatch :: [(Command, Args -> IO ())]
dispatch =
  [ ("add", add)
  , ("view", view)
  , ("remove", remove)
  ]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

add = putStrLn "add"
view = putStrLn "view"
remove = putStrLn "remove"
