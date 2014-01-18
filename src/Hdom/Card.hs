{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Hdom.Card where

import Control.Lens
import Data.Default

import Hdom.Types


instance Default CardAttr where
  def = CardAttr 0 Basic (Nothing, Nothing, Nothing, Nothing)

kingdom = def & cardRole .~ Kingdom


copper   = def & treasure ?~ (Treasure { _coins = 1 })
silver   = def & costs .~ 3
               & treasure ?~ (Treasure { _coins = 2 })
gold     = def & costs .~ 6
               & treasure ?~ (Treasure { _coins = 3 })
estate   = def & costs .~ 2
               & victory ?~ (Victory { _victoryVP = 1 })
duchy    = def & costs .~ 5
               & victory ?~ (Victory { _victoryVP = 3 })
province = def & costs .~ 8
               & victory ?~ (Victory { _victoryVP = 6 })
curse'   = def & curse ?~ (Curse { _curseVP = -1 })

