{-# LANGUAGE Arrows #-}

module MidiPanel (
    midiPanel
  ) where

import Euterpea
import FRP.UISF
import HSoM
import MidiConverterPanel


midiPanel :: UISF () (Maybe OutputDeviceID, Maybe [MidiMessage], Maybe [MidiMessage])
midiPanel = topDown $ setSize (360, 550) $ proc _ -> do
    (mo, mi) <- getDeviceIDs -< ()
    miM <- midiIn -< mi
    _ <- title "Midi in" display -< miM
    out <- midiConverterPanel -< (1, miM)

    returnA -< (mo, miM, out)


getDeviceIDs :: UISF () (Maybe OutputDeviceID, Maybe InputDeviceID)
getDeviceIDs = topDown $
  proc () -> do
    mi <- selectInput -< ()
    mo <- selectOutput -< ()
    outA -< (mo, mi)
