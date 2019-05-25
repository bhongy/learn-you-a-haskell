-------------------
-- 09. Input and Output (05. Exceptions)
-- http://learnyouahaskell.com/input-and-output
-------------------

{- To test all these functions. Just run them in ghci -}

import           System.Directory   (doesFileExist)
import           System.Environment (getArgs)
import           System.IO.Error    (catchIOError, ioeGetFileName,
                                     isDoesNotExistError)

-- test in ghci by `:run tellFileExist <filename>`
-- e.g. `:run tellFileExist package.yaml`
tellFileExist = do
  (filename:_) <- getArgs
  fileExists <- doesFileExist filename
  if fileExists
    then do
      contents <- readFile filename
      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
    else putStrLn "The file doesn't exist!"

tellFileExist2 = toTry `catchIOError` handleError
  where
    toTry :: IO ()
    toTry = do
      (filename:_) <- getArgs
      contents <- readFile filename
      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
    handleError :: IOError -> IO ()
    {- catching everything is bad practice
       ^ causing your program to fail silently when it shouldn't
       catch (recover) what we know how to and let the other errors through
    -}
    -- handleError e = putStrLn "Whoops, had some trouble!"
    handleError e
      | isDoesNotExistError e = case ioeGetFileName e of
        Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
        Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
      | otherwise = ioError e
