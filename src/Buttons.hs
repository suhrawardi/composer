{-# LANGUAGE Arrows #-}

module Buttons (
    buttonsPanel,
  ) where

import Data.Maybe (isJust)
import FRP.UISF


buttonsPanel :: UISF () Bool
buttonsPanel = title "Start/Stop" $ leftRight $ proc _ -> do
    isPlaying <- checkbox "Playing" False -< ()
    returnA -< isPlaying
