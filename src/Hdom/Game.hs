{-# LANGUAGE TemplateHaskell #-}
module Hdom.Game
       ( playTurn,
         action,
         buy,
         cleanup
       ) where

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
        checkBuys
        lift $ showBuyInfo
        c <- MaybeT $ selectCard
        checkBuyable c
        gainCard c

      checkBuys :: MaybeT GameState ()
      checkBuys = MaybeT $ uses (my.turn.buys) checkRemain

      showBuyInfo :: GameState ()
      showBuyInfo = do
        myNo <- use me
        b <- use $ my.turn.buys
        m <- use $ my.turn.money
        lift $ notice [myNo] $ "Money: " ++ show m ++ " / Buys: " ++ show b

      selectCard :: GameState (Maybe Card)
      selectCard = do
        myNo <- use me
        rfs <- use $ field.cards
        let fcards = M.filter (>0) rfs
        let fcl = (Nothing,"End Buy") : zip
                  (fmap Just $ M.keys fcards)
                  (M.elems $ M.mapWithKey (\(Card k) a -> concat [k,"(",show a,")"]) fcards)
        out <- lift $ select [myNo] "Buy Card ?" $ map (^._2) fcl
        return $ fst $ fcl !! (out IM.! myNo)

      checkBuyable :: Card -> MaybeT GameState ()
      checkBuyable c = do
        fcards <- lift $ use $ field.cards
        gcoins <- lift $ use $ my.turn.money
        liftT $ isBuyable fcards gcoins c

      gainCard :: Card -> MaybeT GameState ()
      gainCard c = do
        ca <- liftT $ getCardAttr c
        lift $ do
          my.turn.discards %= (c:)
          my.turn.buys -= 1
          my.turn.money -= ca^.costs
        n <- MaybeT $ uses (field.cards) $ flip restOfCards c
        lift $ field.cards.(at c) ?= n - 1

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

checkRemain :: Int -> Maybe ()
checkRemain r = guard $ r > 0

