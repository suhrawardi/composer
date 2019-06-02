{-# LANGUAGE Arrows #-}

module MainUI (
    runMainUI
  ) where

import ChannelPanel
import Euterpea
import FRP.UISF
import HSoM
import MidiPanel


runMainUI = runMUI (styling "Composer" (2400, 550)) mainUI


mainUI :: UISF () ()
mainUI = leftRight $ proc _ -> do

  (mo, miM, out1) <- midiPanel -< ()

  out2 <- channelPanel -< (2, miM)

  out3 <- channelPanel -< (3, miM)

  out4 <- channelPanel -< (4, miM)

  out5 <- channelPanel -< (5, miM)

  midiOut -< (mo, mappend (mappend (mappend (mappend out1 out2) out3) out4) out5)


styling :: String -> (Int, Int) -> UIParams
styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
