{-# LANGUAGE Arrows #-}

module DecayPanel (
    decayPanel
  ) where

import Data.Maybe (mapMaybe, isJust)
import Euterpea
import HSoM
import FRP.UISF
import System.Random


decay :: MidiMessage -> Int -> Maybe MidiMessage
decay m i =
  let f c k v d = if v > 0
                  then let v' = v - truncate(127 / fromIntegral i)
                       in Just (ANote c k v' d)
                  else Nothing

  in case m of
    ANote c k v d      -> f c k v d
    _                  -> Nothing


maybeDecay :: Int -> MidiMessage -> Maybe MidiMessage
maybeDecay i (Std (NoteOn c k v)) = decay (ANote c k v (1/fromIntegral i)) i
maybeDecay i (ANote c k v _)      = decay (ANote c k v (1/fromIntegral i)) i
maybeDecay _ _                    = Nothing


decayPanel :: UISF (Maybe [MidiMessage]) (Maybe [MidiMessage])
decayPanel = title "Feedback" $ topDown $ proc m -> do
    i <- withDisplay (hiSlider 1 (0, 20) 0) -< ()

    if i == 0
      then
        returnA -< m
      else do
        rec s <- vdelay -< (dur, fmap (mapMaybe (maybeDecay i)) m')
            let m' = mappend m s
                dur = 2/fromIntegral i
        returnA -< s
