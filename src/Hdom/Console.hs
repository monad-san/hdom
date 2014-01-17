{-# LANGUAGE TemplateHaskell, DeriveFunctor, TupleSections #-}
module Hdom.Console where

import Control.Monad(join)
import Data.Monoid(mconcat)
import Data.Maybe(catMaybes, fromMaybe)
import Data.Traversable(sequenceA)
import qualified Data.IntMap as IM
import Control.Lens((^.))
import Control.Monad.Free.Church

import Hdom.PlayerIO
import Hdom.Util(maybeRead)

data ConsoleF a = Notice Keys String a
                | NoticeAll String a
                | Ask Keys String (IM.IntMap String -> a)
                | Select Keys String [String] (IM.IntMap Int -> a)
                | LiftIO (IO a)
                | GameEnd (String,[(String,Int)])
                deriving Functor

type Console = F ConsoleF


notice :: Keys -> String -> Console ()
notice ns s = wrap $ Notice ns s $ return ()

noticeAll :: String -> Console ()
noticeAll s = wrap $ NoticeAll s $ return () 

ask :: Keys -> String -> Console (IM.IntMap String)
ask ns s = wrap $ Ask ns s return

select :: Keys -> String -> [String] -> Console (IM.IntMap Int)
select ns s sl = wrap $ Select ns s sl return

liftIO :: IO a -> Console a
liftIO m = wrap.LiftIO $ fmap return m


runStdConsole :: IM.IntMap PlayerIO -> Console a -> IO a
runStdConsole ps m_ = runF m_ return d where
  d (Notice ns s co) = do
    comb ns $ \p -> p^.pout $ s
    co
  d (NoticeAll s co) = do
    comb (IM.keys ps) $ \p -> p^.pout $ s
    co
  d (Ask ns s co) = do
    ss <- comb ns $ \p -> (p^.pout $ s)>>(p^.pin)
    co ss
  d (Select ns s sl co) = do
    sn <- comb ns $ \p -> do
      p^.pout $ s
      (t,ss) <- return $ foldl (\(n,r) se -> (succ n, concat [r," ",show n,": ",se," |"])) (0,"|") sl
      p^.pout $ ss
      loop (p^.pin) t
    co sn
  d (LiftIO m) = join m
  d (GameEnd (s,score)) = undefined

  comb :: Keys -> (PlayerIO -> IO a) -> IO (IM.IntMap a)
  comb ns f = sequenceA.(fmap f) $ sps ns

  sps :: Keys -> IM.IntMap PlayerIO
  sps ns = IM.fromList $ catMaybes
           $ map (\n -> (IM.lookup n ps)>>=(return.(n,))) ns

  loop :: IO String -> Int -> IO Int
  loop input t = do
    rs <- input
    fromMaybe (loop input t) $ fmap return $ do
      sn <- ((maybeRead rs)::(Maybe Int))
      if 0 <= sn && sn <= t then Just sn else Nothing

