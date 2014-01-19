{-# LANGUAGE TemplateHaskell #-}
module Hdom.Game
       ( playTurn,
         action,
         buy,
         cleanup
       ) where

import Data.Maybe(isJust)
import Data.List(delete)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.State

import Hdom.Types
import qualified Hdom.Console as C
import Hdom.CardInfo
import Hdom.Turn

-- ABC

playTurn :: GameState ()
playTurn = do
  action
  buy
  cleanup

action :: GameState ()
action = do
  lift $ C.noticeAll "Action Phase: Start."
  runMaybeT $ sequence_ $ repeat playAction
  lift $ C.noticeAll "Action Phase: Done."
    where
      playAction :: MaybeT GameState ()
      playAction = do
        checkActions
        lift $ showActionInfo
        c <- MaybeT $ selectActionCard
        execAction c

      checkActions :: MaybeT GameState ()
      checkActions = MaybeT $ do
        uses (my.turn.actions) checkRemain
        uses (my.turn.hands) (checkRemain.length.fst.(extractCards "Action"))

      showActionInfo :: GameState ()
      showActionInfo = do
        myNo <- use me
        a <- use $ my.turn.actions
        lift $ C.notice [myNo] $ concat ["Actions: ",show a]

      selectActionCard :: GameState (Maybe Card)
      selectActionCard = do
        myNo <- use me
        hacs <- use $ my.turn.hands.(to $ fst.(extractCards "Action"))
        let hcl = (Nothing,"End Action") : zip (map Just hacs) (map (op Card) hacs)
        out <- lift $ C.select [myNo] "Play Action ?" $ map (^._2) hcl
        return $ fst $ hcl !! (out IM.! myNo)

      execAction :: Card -> MaybeT GameState ()
      execAction c = do
        lift $ do
          my.turn.hands %= delete c
          my.turn.play %= (++[c])
          my.turn.actions -= 1
        exe <- liftT $ do
          ca <- getCardAttr c
          mac <- ca^.actionCard
          Just $ mac^.effect
        lift $ exe

buy :: GameState ()
buy = do
  lift $ C.noticeAll "Buy Phase: Start."
  playTreasure
  runMaybeT $ sequence_ $ repeat buying
  lift $ C.noticeAll "Buy Phase: Done."
    where
      playTreasure :: GameState ()
      playTreasure = do
        lift $ C.noticeAll " - Played treasures..."
        hs <- use $ my.turn.hands
        let (t,nt) = extractCards "Treasure" hs
        my.turn.hands .= nt
        my.turn.play %= (++t)
        my.turn.money += (sum $ map gainCoins t)

      buying :: MaybeT GameState ()
      buying = do
        checkBuys
        lift $ showBuyInfo
        c <- MaybeT $ selectBuyCard
        checkBuyable c
        gainCard c

      checkBuys :: MaybeT GameState ()
      checkBuys = MaybeT $ uses (my.turn.buys) checkRemain

      showBuyInfo :: GameState ()
      showBuyInfo = do
        myNo <- use me
        b <- use $ my.turn.buys
        m <- use $ my.turn.money
        lift $ C.notice [myNo] $ concat ["Money: ",show m," / Buys: ",show b]

      selectBuyCard :: GameState (Maybe Card)
      selectBuyCard = do
        myNo <- use me
        rfs <- use $ field.cards
        let fcards = M.filter (>0) rfs
        let fcl = (Nothing,"End Buy") : zip
                  (fmap Just $ M.keys fcards)
                  (M.elems $ M.mapWithKey (\(Card k) a -> concat [k,"(",show a,")"]) fcards)
        out <- lift $ C.select [myNo] "Buy Card ?" $ map (^._2) fcl
        return $ fst $ fcl !! (out IM.! myNo)

      checkBuyable :: Card -> MaybeT GameState ()
      checkBuyable c = do
        fcards <- lift $ use $ field.cards
        gcoins <- lift $ use $ my.turn.money
        if isJust $ maybeBuyable fcards gcoins c
          then return ()
          else buying

      maybeBuyable :: FieldMap -> Int -> Card -> Maybe ()
      maybeBuyable fcs m c = do
        ca <- getCardAttr c
        n <- restOfCards fcs c
        guard $ m >= (ca^.costs) && n > 0

      gainCard :: Card -> MaybeT GameState ()
      gainCard c = do
        myNo <- lift $ use me
        lift $ lift $ C.notice [myNo] $ "Bought :" ++ (op Card c)
        ca <- liftT $ getCardAttr c
        lift $ do
          my.turn.discards %= (c:)
          my.turn.buys -= 1
          my.turn.money -= ca^.costs
        n <- MaybeT $ uses (field.cards) $ flip restOfCards c
        lift $ field.cards.(at c) ?= n - 1

cleanup :: GameState ()
cleanup = do
  lift $ C.noticeAll "Cleanup Phase..."
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

