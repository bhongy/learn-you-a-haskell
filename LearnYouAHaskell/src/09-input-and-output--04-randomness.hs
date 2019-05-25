-------------------
-- 09. Input and Output (04. Randomness)
-- http://learnyouahaskell.com/input-and-output
-------------------

{- To test all these functions. Just run them in ghci -}

import           Control.Lens.Operators ((<&>))
import           Control.Monad          (unless)
import           System.Random

-- random :: (RandomGen g, Random a) => g -> (g, a)

rdm :: (Int, StdGen)
rdm = random (mkStdGen 123)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, g1) = random gen
      (secondCoin, g2) = random g1
      (thirdCoin, g3) = random g2
  in (firstCoin, secondCoin, thirdCoin)

rdms :: Int -> [Int]
rdms n = take n $ randoms (mkStdGen 456)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
  let (value, newGen) = random gen
  in value:randoms' newGen

finiteRandoms :: (Integral n, RandomGen g, Random a) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (head, newGen) = random gen
      (tail, finalGen) = finiteRandoms (n - 1) newGen
  in (head:tail, finalGen)

rdm10to20 :: Int -> (Int, StdGen)
rdm10to20 = randomR (10, 20) . mkStdGen

rdm20Alphas :: Int -> String
rdm20Alphas = take 20 . randomRs ('a', 'z') . mkStdGen

ioRandomAlphas = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)

ioRandomAlphas2 = do
  gen <- getStdGen
  let all = randomRs ('a', 'z') gen
      (first, rest) = splitAt 20 all
      (second, _) = splitAt 20 rest
  putStrLn first
  putStrLn second
  putStrLn $ take 40 all

ioRandomAlphasNew =
  getStdGen <&> f
  >>= putStrLn
  >> newStdGen <&> f
  >>= putStrLn
  where
    f = take 20 . randomRs ('a', 'z')

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randomNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  -- exit function if not providing input
  unless (null numberString) $ do
    -- `read` will throw if parse fails
    -- use `reads` instead to give back empty list if parse fail
    let guess = read numberString
    if guess == randomNumber
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randomNumber
    askForNumber newGen

askForNumber2 :: IO ()
askForNumber2 = do
  gen <- getStdGen
  let (randomNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  unless (null numberString) $ do
    let guess = read numberString
    if guess == randomNumber
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randomNumber
    newStdGen -- side effect, replace global random generator
    askForNumber2
