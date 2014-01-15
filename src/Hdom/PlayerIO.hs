{-# LANGUAGE TemplateHaskell #-}
module Hdom.PlayerIO where

import Control.Lens(makeLenses)

data PlayerIO = PlayerIO {
  _pout :: String -> IO (),
  _pin :: IO String
  }
makeLenses ''PlayerIO

