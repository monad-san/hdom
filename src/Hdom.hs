
module Hdom where

import Data.IntMap(fromList)
import Control.Monad.State(evalStateT)

import Hdom.PlayerIO
import Hdom.Console(runStdConsole)
import Hdom.Types
import Hdom.Field
import Hdom.Turn(initial)
import Hdom.Game


selfTurn :: Turn
selfTurn = initial

self :: Player
self = Player "Self" selfTurn

selfIO :: PlayerIO
selfIO = PlayerIO putStrLn getLine

game :: Game
game = Game (basicCardsField 1) (1,self) (fromList [])

main :: IO ()
main = runStdConsole (fromList [(1,selfIO)]) $ flip evalStateT game $ do
  cleanup
  sequence_ $ repeat playTurn
  return ()

