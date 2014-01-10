{-# LANGUAGE DeriveFunctor #-}
module Hdom.Console where

import qualified Data.IntMap as IM
import Control.Monad.Free
import Hdom.Types

data ConsoleF c = Notice String c
                | Select String (IM.IntMap String -> c)
                | TurnStart (Player -> c)
                | GameEnd (String,[(String,Int)])
                deriving Functor

type Console = Free ConsoleF

runStdConsole :: Console a -> IO (Console a)
runStdConsole (Pure a) = return (Pure a)
runStdConsole (Free (Notice s f)) = undefined
runStdConsole (Free (Select s co)) = undefined
runStdConsole (Free (TurnStart co)) = undefined
runStdConsole (Free (GameEnd (s,score))) = undefined

