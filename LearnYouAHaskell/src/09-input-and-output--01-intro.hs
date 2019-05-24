-------------------
-- 09. Input and Output (01. Introduction)
-- http://learnyouahaskell.com/input-and-output
-------------------

{- To test all these functions. Just run them in ghci -}

import Data.Char
import Control.Lens.Operators ((<&>))

greeting :: IO ()
{-
greeting = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"
-}
-- greeting = askName >> getLine >>= (\name -> putStrLn $ formatReply name)
-- greeting = askName >> getLine >>= (putStrLn . formatReply)
-- <&> (Lens) is <$> with the arguments flipped `f a -> (a -> b) -> f b`
greeting = askName >> getLine <&> formatReply >>= putStrLn
  where
    askName :: IO String
    askName = putStrLn "Hello, what's your name?" >> getLine
    formatReply name = "Hey " ++ name ++ ", you rock!"

greeting2 :: IO ()
greeting2 = do
  putStrLn "What's your firstname?"
  firstname <- getLine
  putStrLn "What's your lastname?"
  lastname <- getLine
  let upperFirst = map toUpper firstname
      upperLast = map toUpper lastname
  putStrLn $ "Hey " ++ upperFirst ++ " " ++ upperLast ++ ", how are you?"

greeting2_lambda :: IO ()
greeting2_lambda =
  ask "firstname"
  >>= \ firstname ->
    ask "lastname"
    >>= \ lastname ->
      putStrLn $ formatReply (firstname, lastname)
  where
    ask :: String -> IO String
    ask what = putStrLn ("Hello, what's your " ++ what ++ "?") >> getLine
    upper :: String -> String
    upper = map toUpper
    formatReply :: (String, String) -> String
    formatReply (first, last) =
      "Hey " ++ upper first ++ " " ++ upper last ++ ", how are you?"

printReversedWords :: IO ()
printReversedWords = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      printReversedWords
  -- `words :: String -> [String]` turns string to array of words
  where
    reverseWords = unwords . map reverse . words

{-
hellYeah = do
  a <- return "hell"
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b
-}

-- `let` bindings can be used in `do` block
hellYeah = do
  let a = "hell"
      b = "yeah!"
  putStrLn $ a ++ " " ++ b

putStr' :: String -> IO ()
putStr' [] = return ()
-- putStr' (x:xs) = putChar x >> putStr' xs
putStr' (x:xs) = do
  putChar x
  putStr' xs

getManyLines = do
  rs <- sequence [getLine, getLine, getLine]
  print rs

-- when
-- sequence (map print [1,2,3,4,5])
-- mapM print [1,2,3]
-- mapM_ print [1,2,3]
-- forM [1,2,3] (\a -> do ... return ...)
