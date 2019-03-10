{-# LANGUAGE OverloadedStrings #-}

module FiniteStateMachine.PaymentProvider where

-- https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-IO-Class.html
import Control.Monad.IO.Class
import Data.Semigroup
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- unpack all exports, e.g. Card, Price
import FiniteStateMachine.Checkout

chargeCard :: MonadIO m => Card -> Price -> m ()
chargeCard (Card card) price =
  -- liftIO :: IO a -> m a
  liftIO (T.putStrLn ("Charging card " <> card <> " $" <> T.pack (show price)))
