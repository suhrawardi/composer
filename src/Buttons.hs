{-# LANGUAGE Arrows #-}

module Buttons (
    buttonsPanel,
  ) where

import Data.Maybe (isJust)
import FRP.UISF


buttonsPanel :: UISF () (Bool, Bool)
buttonsPanel = title "Start/Stop" $ leftRight $ proc _ -> do
    isPlaying <- checkbox "Play" False -< ()
    isLearning <- checkbox "Learn" False -< ()
    returnA -< (isPlaying, isLearning)
