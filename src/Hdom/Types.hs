{-# LANGUAGE TemplateHaskell, FunctionalDependencies, OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Hdom.Types where

import Data.String(IsString,fromString)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State

import Hdom.Console(Console)


newtype Card = Card String
             deriving (Eq, Ord)
instance IsString Card where
  fromString s = Card s
instance Show Card where
  show (Card s) = s  
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
instance CardType ActionCard where
  cardType _ = "Action"
instance Show ActionCard where
  show = cardType
makeLenses ''ActionCard

data Treasure = Treasure {
  _coins :: Int
  } deriving (Show)
instance CardType Treasure where
  cardType _ = "Treasure"
makeLenses ''Treasure

newtype Curse = Curse { _curseVP :: Int } deriving (Show)
instance CardType Curse where
  cardType = show
makeFields ''Curse

data Victory = Victory {
  _victoryVP :: Int
  }
instance CardType Victory where
  cardType _ = "Victory"
instance Show Victory where
  show = cardType
makeFields ''Victory


data CardRole = Basic | Kingdom
              deriving (Eq, Ord, Show)

data CardAttr = CardAttr {
  _costs :: Int,
  _cardRole :: CardRole,
  _attributes :: ( Maybe ActionCard
                , Maybe Treasure
                , Maybe Victory
                , Maybe Curse
                )
  } deriving (Show)
makeLenses ''CardAttr

type CardInfo = M.Map String CardAttr

actionCard :: Lens' CardAttr (Maybe ActionCard)
actionCard = attributes._1

treasure :: Lens' CardAttr (Maybe Treasure)
treasure = attributes._2

victory :: Lens' CardAttr (Maybe Victory)
victory = attributes._3

curse :: Lens' CardAttr (Maybe Curse)
curse = attributes._4

