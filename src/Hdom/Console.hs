{-# LANGUAGE TemplateHaskell, DeriveFunctor, TupleSections #-}
module Hdom.Console where

import Control.Monad(join)
import Data.Maybe(catMaybes)
import Data.Traversable(sequenceA)
import qualified Data.IntMap as IM
import Control.Lens((^.))
import Control.Monad.Free.Church

import Hdom.PlayerIO


data ConsoleF a = Notice [Int] String a
                | NoticeAll String a
                | Select [Int] String (IM.IntMap String -> a)
                | LiftIO (IO a)
                | GameEnd (String,[(String,Int)])
                deriving Functor

type Console = F ConsoleF


notice :: [Int] -> String -> Console ()
notice ns s = wrap $ Notice ns s $ return ()

noticeAll :: String -> Console ()
noticeAll s = wrap $ NoticeAll s $ return () 

select ::  [Int] -> String -> Console (IM.IntMap String)
select ns s = wrap $ Select ns s return

liftIO :: IO a -> Console a
liftIO m = wrap.LiftIO $ fmap return m


runConsole :: IM.IntMap PlayerIO -> Console a -> IO a
runConsole ps m_ = runF m_ return d where
  d (Notice ns s co) = do
    comb ns $ \p -> p^.pout $ s
    co
  d (NoticeAll s co) = do
    comb (IM.keys ps) $ \p -> p^.pout $ s
    co
  d (Select ns s co) = do
    ss <- comb ns $ \p -> (p^.pout $ s)>>(p^.pin)
    co ss
  d (LiftIO m) = join m
  d (GameEnd (s,score)) = undefined
  
  comb :: [Int] -> (PlayerIO -> IO a) -> IO (IM.IntMap a)
  comb ns f = sequenceA.(fmap f) $ sps ns
  
  sps :: [Int] -> IM.IntMap PlayerIO
  sps ns = IM.fromList $ catMaybes
           $ map (\n -> (IM.lookup n ps)>>=(return.(n,))) ns

