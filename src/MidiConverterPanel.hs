{-# LANGUAGE Arrows #-}

module MidiConverterPanel (
    midiConverterPanel
  ) where

import Buttons
import Data.Maybe (mapMaybe, isJust)
import Euterpea
import HSoM
import FRP.UISF
import System.Random


convert :: Int -> Time -> MidiMessage -> Maybe MidiMessage
convert channel dur (Std (NoteOn c k v))   = Just (ANote channel k v dur)
convert channel dur (Std (PitchWheel c p)) = Just (Std (PitchWheel channel p))
convert _       _   _                      = Nothing


midiConverterPanel :: UISF (Int, Maybe [MidiMessage]) (Maybe [MidiMessage])
midiConverterPanel = topDown $ setSize (500, 50) $ proc (channel, miM) -> do
    isPlaying <- buttonsPanel >>> handleButtons -< ()
    rec s <- vdelay -< (0.1, fmap (mapMaybe (convert channel 1)) miM')
        let miM' = mappend miM s

    if isPlaying
      then returnA -< miM'
      else returnA -< Nothing

