{-# LANGUAGE DeriveFunctor #-}
module Hdom.Console where

import Control.Monad(join)
import Data.Maybe(catMaybes)
import Data.Traversable(sequenceA)
import qualified Data.IntMap as IM
import Control.Lens
import Control.Monad.Free.Church
import Hdom.Types

data ConsoleF a = Notice [Int] String a
                | Select [Int] String (IM.IntMap String -> a)
                | LiftIO (IO a)
                | GameEnd (String,[(String,Int)])
                deriving Functor

type Console = F ConsoleF

notice ns s = wrap $ Notice ns s $ return ()
select ns s = wrap $ Select ns s return

runConsole :: IM.IntMap Player -> Console a -> IO a
runConsole ps m_ = runF m_ return d where
  d (Notice ns s co) = do
    comb ns $ \p -> p^.pout $ s
    co
  d (Select ns s co) = do
    ss <- comb ns $ \p -> (p^.pout $ s)>>(p^.pin)
    co ss
  d (LiftIO m) = join m
  d (GameEnd (s,score)) = undefined
  
  comb :: [Int] -> (Player -> IO a) -> IO (IM.IntMap a)
  comb ns f = sequenceA.(fmap f) $ sps ns
  
  sps :: [Int] -> IM.IntMap Player
  sps ns = IM.fromList $ catMaybes
           $ map (\n -> (IM.lookup n ps)>>=(\p -> return (n,p))) ns

