{-# LANGUAGE Arrows #-}

module ChannelPanel (
    channelPanel
  ) where

import Buttons
import Data.Maybe (mapMaybe, isJust)
import Euterpea
import HSoM
import FRP.UISF


channelPanel :: UISF (Int, Maybe [MidiMessage]) (Maybe [MidiMessage])
channelPanel = topDown $ setSize (400, 600) $ proc (channel, miM) -> do
    isPlaying <- buttonsPanel >>> handleButtons -< ()
    moM <- delayPanel -< miM

    let moM' = if isPlaying && isJust moM
                   then moM
                   else Nothing

    returnA -< moM


decay :: Time -> MidiMessage -> Maybe MidiMessage
decay dur m =
  let f c k v d = if v > 0
                  then let v' = truncate(fromIntegral v * 2.4)
                       in Just (ANote c k v' d)
                  else Nothing
  in case m of
    ANote c k v d      -> f c k v d
    Std (NoteOn c k v) -> f c k v dur
    _                  -> Nothing


delayPanel :: UISF (Maybe [MidiMessage]) (Maybe [MidiMessage])
delayPanel = title "Delay" $ topDown $ proc m -> do
    d <- title "Decay rate" $ withDisplay (hSlider (0, 0.9) 0.1) -< ()
    f <- title "Echo frequency" $ withDisplay (hSlider (0, 10) 0) -< ()
    rec s <- vdelay -< (1/f, fmap (mapMaybe (decay d)) m')
        let m' = mappend m s
    returnA -< s
