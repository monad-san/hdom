{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Hdom.CardInfo where

import qualified Data.Map as M
import Data.Maybe(catMaybes,fromMaybe)
import Data.List(partition)
import Control.Lens

import Hdom.Types
import Hdom.Card
import Hdom.Card.BaseSet

pickCard :: CardInfo -> String -> Card -> Bool
pickCard ci n c = any (==n) $ verify (ci^.(at c))
  where
    verify :: Maybe CardAttr -> [String]
    verify (Just ca) = catMaybes $ (getct (ca^.attributes))^..each
    verify _ = []

    getct a = a & _1 %~ f & _2 %~ f & _3 %~ f & _4 %~ f

    f :: (CardType ct) => Maybe ct -> Maybe String
    f = (>>=(\ct -> Just $ cardType ct))

extractCards' :: CardInfo -> String -> [Card] -> ([Card],[Card])
extractCards' ci n cs = partition (pickCard ci n) cs

gainCoins' :: CardInfo -> Card -> Int
gainCoins' ci c = fromMaybe 0 $ (ci^.(at c))>>=(^.treasure)>>=(\t -> return $ t^.coins)

getCardAttr' :: CardInfo -> Card -> Maybe CardAttr
getCardAttr' ci c = ci^.(at c)

extractCards = extractCards' totalCI
gainCoins = gainCoins' totalCI
getCardAttr = getCardAttr' totalCI


totalCI :: CardInfo
totalCI = M.unions [basicCI,baseSet]

basicCI :: CardInfo
basicCI = M.fromList [("Copper", copper),
                      ("Silver", silver),
                      ("Gold", gold),
                      ("Estate", estate),
                      ("Duchy", duchy),
                      ("Province", province),
                      ("Curse", curse')]

baseSet :: CardInfo
baseSet = M.fromList [("Market", market),
                      ("Village", village),
                      ("Festival", festival)]

