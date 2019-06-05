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

pitchbend 1 C  = 89
pitchbend 1 Cs = 42
pitchbend 1 D  = 61
pitchbend 1 Ds = 37
pitchbend 1 E  = 67
pitchbend 1 F  = 86
pitchbend 1 Fs = 39
pitchbend 1 G  = 58
pitchbend 1 Gs = 45
pitchbend 1 A  = 64
pitchbend 1 As = 83
pitchbend 1 B  = 70

pitchbend 2 C  = 64
pitchbend 2 Cs = 48
pitchbend 2 D  = 70
pitchbend 2 Ds = 55
pitchbend 2 E  = 39
pitchbend 2 F  = 61
pitchbend 2 Fs = 45
pitchbend 2 G  = 67
pitchbend 2 Gs = 52
pitchbend 2 A  = 39
pitchbend 2 As = 58
pitchbend 2 B  = 45

pitchbend _ _  = 64
