{-# LANGUAGE TemplateHaskell #-}
module Hdom.Game where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.State

import Hdom.Types
import Hdom.Console
import Hdom.Card
import Hdom.Turn

-- ABC

playTurn :: GameState ()
playTurn = do
  action
  buy
  cleanup

action :: GameState () -- (stub) --
action = return ()

buy :: GameState ()
buy = do
  lift $ noticeAll "Buy Phase: Start."
  playTreasure
  runMaybeT $ sequence_ $ repeat buying
  lift $ noticeAll "Buy Phase: Done."
    where
      playTreasure :: GameState ()
      playTreasure = do
        lift $ noticeAll " - Played treasures..."
        hs <- use $ my.turn.hands
        let (t,nt) = extractCards "Treasure" hs
        my.turn.hands .= nt
        my.turn.play .= t
        my.turn.money += (sum $ map gainCoins t)

      buying :: MaybeT GameState ()
      buying = do
        g <- lift $ get
        lift $ lift $ notice [g^.me]
          $ "Money: " ++ show (g^.my.turn.money) ++ " / Buys: " ++ show (g^.my.turn.buys)
        liftT $ checkBuys g
        c <- askCard
        ca <- liftT $ getCardAttr c
        lift $ do
          my.turn.discards %= (c:)
          my.turn.buys -= 1
          my.turn.money -= ca^.costs
        n <- liftT $ restOfCards (g^.field.cards) c
        lift $ field.cards.(at c) ?= n - 1

      askCard :: MaybeT GameState Card
      askCard = do
        myNo <- use me
        fcs <- use $ field.cards
        ms <- use $ my.turn.money
        ss <- lift $ lift $ select [myNo] "Buy Card ?"
        cs <- liftT $ if ss == IM.singleton myNo ""
                      then Nothing
                      else ss^.(at myNo)
        let r = do
              c <- selectCard fcs cs
              isBuyable fcs ms c
              return c
        if r == Nothing
          then askCard
          else liftT r
 
      checkBuys :: Game -> Maybe ()
      checkBuys g = guard $ g^.my.turn.buys > 0

      isBuyable :: FieldMap -> Int -> Card -> Maybe ()
      isBuyable fcs m c = do
        ca <- getCardAttr c
        n <- restOfCards fcs c
        guard $ m >= (ca^.costs) && n > 0

cleanup :: GameState ()
cleanup = do
  lift $ noticeAll "Cleanup Phase..."
  t <- use (my.turn)
  post_t <- lift $ flip execStateT t $ do
    discardCards
    resetTurn
    sequence_ $ replicate 5 draw
  my.turn .= post_t

-- Util

liftT :: (Monad m) => Maybe a -> MaybeT m a
liftT = MaybeT . return
      
restOfCards :: FieldMap -> Card -> Maybe Int
restOfCards fcs c = M.lookup c fcs

selectCard :: M.Map Card Int -> String -> Maybe Card
selectCard cs s = let c = Card s
                  in (cs^.(at c))>>=(\n -> if n > 0 then Just c else Nothing)

