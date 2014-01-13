{-# LANGUAGE TemplateHaskell #-}
module Hdom.Types where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State

data CardRole = Basic | Kingdom
              deriving (Eq, Ord)
makeLenses ''CardRole

data Card = Card {
  _cardName :: String,
  _costs :: Int,
  _cardRole :: CardRole
  } deriving (Eq, Ord)
makeClassy ''Card
instance Show Card where
  show c = c^.cardName

data Turn = Turn {
  _money :: Int,
  _actions :: Int,
  _buys :: Int,
  _play :: [Card],
  _deck :: [Card],
  _hands :: [Card],
  _discards :: [Card]
  } deriving (Show, Eq)
makeLenses ''Turn


data Field = Field {
  _cards :: M.Map Card Int,
  _trashed :: [Card]
  } deriving (Show, Eq)
makeLenses ''Field

data Player = Player {
  _nick :: String,
  _turn :: Turn,
  _pin :: IO String,
  _pout :: (String -> IO ())
  }
makeLenses ''Player
instance Show Player where
  show c = c^.(turn.(to show))
instance Eq Player where
  (==) a b = a^.nick == b^.nick


data Game = Game {
  _field :: Field,
  _players :: IM.IntMap Player,
  _active_player :: (Int, Player)
  } deriving (Show, Eq)
makeLenses ''Game


data ActionCard = ActionCard {
  _actionCard :: Card,
  _effect :: StateT Game IO ()
  }
makeLenses ''ActionCard
instance HasCard ActionCard where
  card = actionCard

data TreasureCard = TreasureCard {
  _treasureCard :: Card,
  _coins :: Int
  }
makeLenses ''TreasureCard
instance HasCard TreasureCard where
  card = treasureCard

data VPCard = CurseCard { _vpCard :: Card, _points :: Int }
            | VictoryCard {
  _vpCard :: Card,
  _points :: Int
  }
makeLenses ''VPCard
instance HasCard VPCard where
  card = vpCard

