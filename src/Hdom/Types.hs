{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Hdom.Types where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State

import Hdom.Console(Console)


newtype Card = Card String
             deriving (Eq, Ord, Show)
makeWrapped ''Card

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

type TurnState = StateT Turn Console


data Field = Field {
  _cards :: M.Map Card Int,
  _trashed :: [Card]
  } deriving (Show, Eq)
makeLenses ''Field

data Player = Player {
  _nick :: String,
  _turn :: Turn
  } deriving (Eq)
makeLenses ''Player
instance Show Player where
  show = _nick


data Game = Game {
  _field :: Field,
  _active_player :: (IM.Key,Player),
  _others :: IM.IntMap Player
  } deriving (Show, Eq)
makeLenses ''Game

type GameState = StateT Game Console


class CardType ct where
  cardType :: ct -> String

data ActionCard = ActionCard {
  _effect :: GameState ()
  }
makeLenses ''ActionCard
instance CardType ActionCard where
  cardType _ = "Action"

data Treasure = Treasure {
  _coins :: Int
  }
makeLenses ''Treasure
instance CardType Treasure where
  cardType _ = "Treasure"

newtype Curse = Curse { _curseVP :: Int }
instance CardType Curse where
  cardType _ = "Curse"
makeFields ''Curse

data Victory = Victory {
  _victoryVP :: Int
  }
instance CardType Victory where
  cardType _ = "Victory"
makeFields ''Victory


data CardRole = Basic | Kingdom
              deriving (Eq, Ord, Show)

data CardAttr = CardAttr {
  _costs :: Int,
  _cardRole :: CardRole,
  _attribute :: ( Maybe ActionCard
                , Maybe Treasure
                , Maybe Victory
                , Maybe Curse
                )
  }
makeLenses ''CardAttr
instance Show CardAttr where
  show c = "CardAttr " ++ show (c^.costs) ++ " " ++ show (c^.cardRole)

type CardInfo = M.Map String CardAttr

actionCard :: Lens' CardAttr (Maybe ActionCard)
actionCard = attribute._1

treasure :: Lens' CardAttr (Maybe Treasure)
treasure = attribute._2

victory :: Lens' CardAttr (Maybe Victory)
victory = attribute._3

--curse = attribute._4

