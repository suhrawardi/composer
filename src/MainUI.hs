{-# LANGUAGE Arrows #-}

module MainUI (
    runMainUI
  ) where

import ChannelPanel
import Euterpea
import FRP.UISF
import HSoM
import MidiPanel


runMainUI = runMUI (styling "Composer" (2040, 680)) mainUI


mainUI :: UISF () ()
mainUI = leftRight $ proc _ -> do

  (mo, tuning, miM, out1) <- midiPanel -< ()

  out2 <- channelPanel -< (2, tuning, miM)

  out3 <- channelPanel -< (3, tuning, miM)

  out4 <- channelPanel -< (4, tuning, miM)

  midiOut -< (mo, mappend (mappend (mappend out1 out2) out3) out4)


styling :: String -> (Int, Int) -> UIParams
styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
