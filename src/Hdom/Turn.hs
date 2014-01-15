{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Hdom.Turn where

import Control.Monad.State
import Control.Lens

import Hdom.Types
import Hdom.Util(mkDeck)
import qualified Hdom.Console as C

draw :: TurnState ()
draw = do
  now_deck <- use deck
  now_discards <- use discards
  if (null now_deck) && (not $ null now_discards)
    then shuffle
    else return ()
  new_deck <- use deck
  if null new_deck
    then return ()
    else do deck %= tail
            hands %= ((head $ new_deck):)

shuffle :: TurnState ()
shuffle = do
  pre_deck <- use discards
  shuffled <- lift $ C.liftIO $ mkDeck pre_deck
  deck .= shuffled
  discards .= []

discardCards :: TurnState ()
discardCards = do
  t <- get
  discards %= (++ t^.play ++ t^.hands)
  play .= []
  hands .= []

resetTurn :: TurnState ()
resetTurn = do
  money .= 0
  actions .= 1
  buys .= 1

-- Initial Turn

initial :: Turn
initial = Turn { _money = 0, _actions = 0, _buys = 0
               , _play = [], _deck = [], _hands = []
               , _discards = (take 7 (repeat "Copper"))++(take 3 (repeat "Estate")) }

