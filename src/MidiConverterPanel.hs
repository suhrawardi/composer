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


convertOct :: Int -> Maybe MidiMessage -> Maybe MidiMessage
convertOct _   Nothing         = Nothing
convertOct oct midiMessage     = do
    (ANote c k v d) <- midiMessage
    let (p, _) = pitch c
        ap = absPitch (p, oct)
    Just (ANote ap k v d)


convertMsg :: Int -> MidiMessage -> Maybe MidiMessage
convertMsg channel (Std (NoteOn c k v))   = Just (Std (NoteOn channel k v))
convertMsg channel (Std (NoteOff c k v))  = Just (Std (NoteOff channel k v))
convertMsg channel (Std (PitchWheel c p)) = Just (Std (PitchWheel channel p))
convertMsg _       _                      = Nothing


convert :: Int -> Octave -> MidiMessage -> Maybe MidiMessage
convert channel oct msg = convertOct oct (convertMsg channel msg)


midiConverterPanel :: UISF (Int, Maybe [MidiMessage]) (Maybe [MidiMessage])
midiConverterPanel = topDown $ setSize (400, 150) $ proc (channel, miM) -> do
    isPlaying <- buttonsPanel >>> handleButtons -< ()
    oct <- title "Octave" $ withDisplay (hiSlider 1 (1, 10) 4) -< ()
    rec s <- vdelay -< (0.1, fmap (mapMaybe (convert channel oct)) miM')
        let miM' = mappend miM s

    if isPlaying
      then returnA -< miM'
      else returnA -< Nothing
