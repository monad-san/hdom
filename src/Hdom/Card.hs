{-# LANGUAGE TemplateHaskell #-}
module Hdom.Card where

import qualified Data.Map as M
import Data.Maybe(catMaybes)
import Data.List(partition)
import Control.Monad.State
import Control.Lens
import Data.Default

import Hdom.Types

pickCard :: CardInfo -> String -> Card -> Bool
pickCard ci n (Card c) = any (==n) $ verify (ci^.(at c))
  where
    verify :: Maybe CardAttr -> [String]
    verify (Just ca) = catMaybes $ (getct (ca^.attributes))^..each
    verify _ = []

    getct a = a & _1 %~ f & _2 %~ f & _3 %~ f & _4 %~ f

    f :: (CardType ct) => Maybe ct -> Maybe String
    f = (>>=(\ct -> Just $ cardType ct))

extractCards' :: CardInfo -> String -> [Card] -> ([Card],[Card])
extractCards' ci n cs = partition (pickCard ci n) cs

extractCards = extractCards' cardMap

f :: (CardType ct) => Maybe ct -> Maybe String
f = (>>=(\ct -> Just $ cardType ct))

instance Default CardAttr where
  def = CardAttr 0 Basic (Nothing, Nothing, Nothing, Nothing)

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


cardMap :: M.Map String CardAttr
cardMap = M.fromList [("Copper", copper),
                      ("Silver", silver),
                      ("Gold", gold),
                      ("Estate", estate),
                      ("Duchy", duchy),
                      ("Province", province),
                      ("Curse", curse')]

