{-# LANGUAGE Arrows #-}

module MidiPanel (
    midiPanel
  ) where

import Debugger
import Euterpea
import FRP.UISF
import HSoM


midiPanel :: UISF () (Maybe OutputDeviceID, Maybe [MidiMessage])
midiPanel = topDown $ setSize (500, 401) $ proc _ -> do
    (mo, mi) <- getDeviceIDs -< ()
    miM <- midiIn -< mi
    _ <- title "Midi in" display -< maybeTrace miM

    returnA -< (mo, miM)


getDeviceIDs :: UISF () (Maybe OutputDeviceID, Maybe InputDeviceID)
getDeviceIDs = topDown $
  proc () -> do
    mi <- selectInput -< ()
    mo <- selectOutput -< ()
    outA -< (mo, mi)
