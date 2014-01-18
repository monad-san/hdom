{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Hdom.Card.BaseSet where

import Control.Monad.State
import Control.Lens
import Data.Default

import Hdom.Types
import Hdom.Card
import Hdom.Turn

market   = kingdom & costs .~ 5
                   & actionCard ?~ (ActionCard { _effect = market' })
  where
    market' = do
      my.turn.actions += 1
      my.turn.money += 1
      my.turn.buys += 1
      t <- use (my.turn)
      nt <- lift $ execStateT draw t
      my.turn .= nt

village  = kingdom & costs .~ 3
                   & actionCard ?~ (ActionCard { _effect = village' })
  where
    village' = do
      my.turn.actions += 2
      t <- use (my.turn)
      nt <- lift $ execStateT draw t
      my.turn .= nt

festival = kingdom & costs .~ 5
                   & actionCard ?~ (ActionCard { _effect = festival' })
  where
    festival' = do
      my.turn.actions += 2
      my.turn.money += 2
      my.turn.buys += 1

