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
instance Show Card where
  show c = (_cardName c)
makeClassy ''Card

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
  _pin :: (String -> IO String),
  _pout :: (String -> IO ())
  }
instance Show Player where
  show c = show $ _turn c
instance Eq Player where
  (==) a b = (_nick a) == (_nick b) && (_turn a) == (_turn b)
makeLenses ''Player


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

data VPCard = VictoryCard {
  _vpCard :: Card,
  _points :: Int
  }
            | CurseCard {
  _vpCard :: Card,
  _points :: Int
  }
makeLenses ''VPCard
instance HasCard VPCard where
  card = vpCard

