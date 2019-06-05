{-# LANGUAGE Arrows #-}

module MidiPanel (
    midiPanel
  ) where

import Euterpea
import FRP.UISF
import HSoM
import MidiConverterPanel


midiPanel :: UISF () (Maybe OutputDeviceID, Int, Maybe [MidiMessage], Maybe [MidiMessage])
midiPanel = topDown $ setSize (360, 550) $ proc _ -> do
    (mo, mi) <- getDeviceIDs -< ()
    miM <- midiIn -< mi
    _ <- title "Midi in" display -< miM
    tuning <- title "Tuning" $ radio tunings 0 -< ()
    out <- midiConverterPanel -< (1, miM)

    returnA -< (mo, tuning, miM, out)


getDeviceIDs :: UISF () (Maybe OutputDeviceID, Maybe InputDeviceID)
getDeviceIDs = topDown $
  proc () -> do
    mi <- selectInput -< ()
    mo <- selectOutput -< ()
    outA -< (mo, mi)


tunings :: [String]
tunings = ["Chromatic", "Just Intonation", "Pythagorean"]
