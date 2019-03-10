{-# LANGUAGE OverloadedStrings #-}
-- set pragma to OverloadedString for `Text`
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#overloadedstrings

module FiniteStateMachine.StateMachinesWithAdts where

import Control.Monad         (foldM)
import Data.List.NonEmpty
import Data.Text             (Text)
import Text.Printf           (printf)

import qualified FiniteStateMachine.PaymentProvider as PaymentProvider
import FiniteStateMachine.Checkout
  ( Card(..) {- import all value constructors -}
  , CartItem(..)
  , calculatePrice
  )

data CheckoutState
  = NoItems
  | HasItems (NonEmpty CartItem)
  | NoCard (NonEmpty CartItem)
  | CardSelected (NonEmpty CartItem) Card
  | CardConfirmed (NonEmpty CartItem) Card
  | OrderPlaced
  deriving (Show, Eq)

-- Commands (in Event-sourcing world)
data CheckoutEvent
  = Select CartItem
  | Checkout
  | SelectCard Card
  | Confirm
  | PlaceOrder
  | Cancel
  deriving (Show, Eq)

-- type alias
type FiniteStateMachine state event = state -> event -> IO state

{- "checkout" the state reducer (transition) function to IO (effect) of the next state

   `return` is Monad `pure` (a.k.a. `of`)

   `:|` is a constructor for NonEmpty List `a :| [a]`
   `<|` prepend an element to NonEmpty List
   http://hackage.haskell.org/package/semigroups-0.12.2/docs/Data-List-NonEmpty.html
-}

-- checkout :: CheckoutState -> CheckoutEvent -> IO CheckoutState
checkout :: FiniteStateMachine CheckoutState CheckoutEvent

checkout NoItems (Select item) =
  return (HasItems (item :| []))

checkout (HasItems items) (Select item) =
  return (HasItems (item <| items))

checkout (HasItems items) Checkout =
  return (NoCard items)

checkout (NoCard items) (SelectCard card) =
  return (CardSelected items card)

checkout (CardSelected items card) Confirm =
  return (CardConfirmed items card)

checkout state Cancel =
  case state of
    NoCard items           -> return (HasItems items)
    CardSelected items _   -> return (HasItems items)
    CardConfirmed items _  -> return (HasItems items)
    _                      -> return state

-- how do I write this using bind (>>=)?
checkout (CardConfirmed items card) PlaceOrder = do
  -- effect
  PaymentProvider.chargeCard card (calculatePrice items)
  return OrderPlaced

-- default
checkout state _ = return state

{- Running the machine -}

-- s ~ state
-- e ~ event
-- runMachine :: machine -> initialState -> event[] -> IO state
runMachine :: Foldable f => FiniteStateMachine s e -> s -> f e -> IO s
runMachine = foldM

-- `<-` is "draw from"
-- wraps the machine to log each step - take a mchine returns a new machine
withLogging :: (Show s, Show e)
  => FiniteStateMachine s e
  -> FiniteStateMachine s e
withLogging fsm s e = do
  s' <- fsm s e
  printf "- %s x %s -> %s\n" (show s) (show e) (show s')
  return s'
