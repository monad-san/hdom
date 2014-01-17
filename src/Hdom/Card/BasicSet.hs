{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Hdom.Card.BasicSet where

import Control.Monad.State
import Control.Lens
import Data.Default

import Hdom.Types
import Hdom.Card


market   = kingdom & costs .~ 5
                   & actionCard ?~ (ActionCard { _effect = market' })
  where
    market' = do
      my.turn.actions += 1
      my.turn.coins += 1
      my.turn.buys += 1
      new <- uses (my.turn) (lift.(execStateT draw))
      my.turn .= new

village  = kingdom & costs .~ 3
                   & actionCard ?~ (ActionCard { _effect = village' })
  where
    village' = do
      my.turn.actions += 2
      new <- uses (my.turn) (lift.(execStateT draw))
      my.turn .= new

festival = kingdom & costs .~ 5
                   & actionCard ?~ (ActionCard { _effect = festival' })
  where
    festival' = do
      my.turn.actions += 2
      my.turn.coins += 2
      my.turn.buys += 1


basicSet :: CardInfo
basicSet = M.fromList [ ("Market", market)
                      , ("Village", village)
                      , ("Festival", festival)]

