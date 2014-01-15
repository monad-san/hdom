{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Hdom.Field where

import qualified Data.Map as M
import Control.Lens

import Hdom.Types
import Hdom.Card


noneKingdomCardsField :: Int -> Field
noneKingdomCardsField n = Field {
    _cards = M.fromList [("Copper",60-n*7),("Silver",30),("Gold",30),("Estate",vic),("Duchy",vic),("Province",vic),("Curse",10*(n-1))]
  , _trashed = []
  }
  where
    vic = if n > 2 then 12 else 8

