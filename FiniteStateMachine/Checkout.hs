{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module FiniteStateMachine.Checkout where

import Control.Monad.IO.Class
import Data.List.NonEmpty
import Data.Text (Text)
import qualified Data.Text as T

type Price = Double

data CartItem =
  CartItem
  { itemId    :: Text
  , itemPrice :: Price
  }
  deriving (Show, Eq)

newtype Card =
  Card Text
  deriving (Show, Eq)

calculatePrice :: NonEmpty CartItem -> Price
calculatePrice items = fromIntegral (round (foldl go 0 items))
  where
    go sum item = itemPrice item + sum
