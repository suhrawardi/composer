{-# LANGUAGE Arrows #-}

module Tuning (
    adjustTuning
  ) where

import Data.Maybe (mapMaybe)
import Euterpea
import FRP.UISF
import HSoM


adjustTuning :: UISF (Int, Maybe [MidiMessage]) (Maybe [MidiMessage])
adjustTuning = proc (tuning, miM) ->
    if tuning > 0
      then
        returnA -< mappend miM (fmap (mapMaybe (adjust tuning)) miM)
      else
        returnA -< miM


adjust :: Int -> MidiMessage -> Maybe MidiMessage
adjust tuning midiMsg = let pitch = pitchbendFromMsg tuning midiMsg
                            channel = channelFromMsg midiMsg
                        in if channel > 0
                             then Just (Std (PitchWheel channel pitch))
                             else Nothing


channelFromMsg :: MidiMessage -> Int
channelFromMsg (Std (NoteOn channel _ _)) = channel
channelFromMsg (ANote channel _ _ _)      = channel
channelFromMsg _                          = 0


pitchbendFromMsg :: Int -> MidiMessage -> Int
pitchbendFromMsg 0      _                      = 64
pitchbendFromMsg tuning (Std (NoteOn _ key _)) =
    let (p, _) = pitch key
    in pitchbend tuning p
pitchbendFromMsg tuning (ANote _ key _ _)      =
    let (p, _) = pitch key
    in pitchbend tuning p


pitchbend :: Int -> PitchClass -> Int

pitchbend 1 C  = 69
pitchbend 1 Cs = round 59.625
pitchbend 1 D  = round 63.375
pitchbend 1 Ds = round 58.6875
pitchbend 1 E  = round 64.625
pitchbend 1 F  = round 68.375
pitchbend 1 Fs = 59
pitchbend 1 G  = round 62.75
pitchbend 1 Gs = round 60.25
pitchbend 1 A  = 64
pitchbend 1 As = round 67.75
pitchbend 1 B  = round 65.25

pitchbend 2 C  = 64
pitchbend 2 Cs = round 60.875
pitchbend 2 D  = round 65.25
pitchbend 2 Ds = round 62.125
pitchbend 2 E  = 59
pitchbend 2 F  = round 63.375
pitchbend 2 Fs = round 60.25
pitchbend 2 G  = round 64.625
pitchbend 2 Gs = round 61.5
pitchbend 2 A  = 59
pitchbend 2 As = round 62.75
pitchbend 2 B  = round 60.25

pitchbend _ _  = 64
