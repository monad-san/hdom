
module Hdom.Util
       (head',
        mkDeck,
        maybeRead) where

import Data.Maybe(listToMaybe)
import qualified Data.Map as M
import System.Random

head' :: [a] -> Maybe a
head' (c:_) = Just c
head' []     = Nothing

mkDeckStep :: RandomGen g => (M.Map Int a, g) -> (Int, a) -> (M.Map Int a, g)
mkDeckStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

mkDeck :: [a] -> IO [a]
mkDeck deck = do
  g <- newStdGen
  return $ fst $ mkDeck' g deck
    where
      mkDeck' :: RandomGen g => g -> [a] -> ([a], g)
      mkDeck' gen [] = ([], gen)
      mkDeck' gen l  = toElems $ foldl mkDeckStep (initial (head l) gen) (numerate (tail l))
      toElems (x, y) = (M.elems x, y)
      numerate = zip [1..]
      initial x gen = (M.singleton 0 x, gen)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

