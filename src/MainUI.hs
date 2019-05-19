{-# LANGUAGE Arrows #-}

module MainUI (
    runMainUI
  ) where

import ChannelPanel
import Euterpea
import FRP.UISF
import HSoM
import MidiPanel


runMainUI = runMUI (styling "Composer" (2000, 800)) mainUI


mainUI :: UISF () ()
mainUI = leftRight $ proc _ -> do

  (mo, miM) <- midiPanel -< ()

  out1 <- channelPanel -< (1, miM)
  midiOut -< (mo, out1)

  out2 <- channelPanel -< (2, miM)
  midiOut -< (mo, out2)

  out3 <- channelPanel -< (3, miM)
  midiOut -< (mo, out3)


styling :: String -> (Int, Int) -> UIParams
styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
