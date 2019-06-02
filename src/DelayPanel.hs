{-# LANGUAGE Arrows #-}

module DelayPanel (
    delayPanel
  ) where

import Data.Maybe (mapMaybe, isJust)
import Euterpea
import HSoM
import FRP.UISF
import System.Random


decay :: MidiMessage -> Maybe MidiMessage
decay m =
  let f c k v d = if v > 0
                  then let v' = truncate(fromIntegral v * 2.4)
                       in Just (ANote c k v' d)
                  else Nothing

  in case m of
    ANote c k v d      -> f c k v d
    _                  -> Nothing


maybeDecay :: Time -> MidiMessage -> Maybe MidiMessage
maybeDecay dur (Std (NoteOn c k v)) = decay (ANote c k v dur)
maybeDecay dur (ANote c k v _)  = decay (ANote c k v dur)
maybeDecay _   _     = Nothing


delayPanel :: UISF (Double, Maybe [MidiMessage]) (Maybe [MidiMessage])
delayPanel = title "Channel" $ topDown $ proc (dur, m) -> do
    f <- title "Echo frequency" $ withDisplay (hSlider (0, 10) 0) -< ()

    if f == 0
      then
        returnA -< m
      else do
        rec s <- vdelay -< (1/f, fmap (mapMaybe (maybeDecay dur)) m')
            let m' = mappend m s
        returnA -< s
