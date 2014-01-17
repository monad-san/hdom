{-# LANGUAGE TemplateHaskell #-}
module Hdom.PlayerIO where

import Data.IntMap(Key)
import Control.Lens(makeLenses)

data PlayerIO = PlayerIO {
  _pout :: String -> IO (),
  _pin :: IO String
  }
makeLenses ''PlayerIO

type Keys = [Key]

