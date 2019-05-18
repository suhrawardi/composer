{-# LANGUAGE Arrows #-}

module Buttons (
    buttonsPanel,
    handleButtons
  ) where

import Data.Maybe (isJust)
import FRP.UISF


buttonsPanel :: UISF () (SEvent (), SEvent ())
buttonsPanel = title "Start/Stop" $ leftRight $ proc _ -> do
    start <- edge <<< button "Start" -< ()
    stop <- edge <<< button "Stop" -< ()
    returnA -< (start, stop)


handleButtons :: UISF (SEvent a, SEvent b) Bool
handleButtons = proc (start, stop) -> do
    rec isPlaying <- delay False -< isPlaying'
        let isPlaying' = case (start, stop) of
                             (Just _, _) -> True
                             (_, Just _) -> False
                             _ -> isPlaying
    returnA -< isPlaying
