{-# LANGUAGE Arrows #-}

module MainUI (
    runMainUI
  ) where

import ChannelPanel
import Euterpea
import FRP.UISF
import HSoM
import MidiConverterPanel
import MidiPanel


runMainUI = runMUI (styling "Composer" (2000, 800)) mainUI


mainUI :: UISF () ()
mainUI = leftRight $ proc _ -> do

  (mo, miM, out1) <- (| topDown ( do
    (mo, miM) <- midiPanel -< ()
    out1 <- midiConverterPanel -< (1, miM)
    returnA -< (mo, miM, out1) ) |)

  out2 <- channelPanel -< (2, miM)

  out3 <- channelPanel -< (3, miM)

  out4 <- channelPanel -< (4, miM)

  midiOut -< (mo, mappend (mappend (mappend out1 out2) out3) out4)


styling :: String -> (Int, Int) -> UIParams
styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
