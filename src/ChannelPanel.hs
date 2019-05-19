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
channelPanel = topDown $ setSize (500, 600) $ proc (channel, miM) -> do
    _ <- title "Channel" display -< channel
    isPlaying <- buttonsPanel >>> handleButtons -< ()
    moM <- delayPanel -< miM

    let moM' = if isPlaying && isJust moM
                   then moM
                   else Nothing

    returnA -< moM


decay :: MidiMessage -> Maybe MidiMessage
decay m =
  let f c k v d = if v > 0
                  then let v' = truncate(fromIntegral v * 2.4)
                       in Just (ANote c k v' d)
                  else Nothing

  in case m of
    ANote c k v d      -> f c k v d
    _                  -> Nothing


delayPanel :: UISF (Maybe [MidiMessage]) (Maybe [MidiMessage])
delayPanel = title "Delay" $ leftRight $ proc m -> do
    notes <- topDown $ setSize (60, 315) $ title "Notes" $ checkGroup notes -< ()

    (d, r, m, f, oct) <- (| topDown ( do
      oct <- title "Octave" $ withDisplay (hiSlider 1 (1, 10) 4) -< ()
      d <- title "Decay rate" $ withDisplay (hSlider (0, 0.9) 0.1) -< ()
      f <- title "Echo frequency" $ withDisplay (hSlider (0, 10) 0) -< ()

      rSeed <- title "Rand" $ withDisplay (hSlider (2.4, 4.0) 2.4) -< ()
      t <- timer -< d
      r <- accum 0.1 -< fmap (const (grow rSeed)) t
      _ <- title "Decay" display -< normalize d r
      returnA -< (d, r, m, f, oct) ) |)

    rec s <- vdelay -< (1/f, fmap (mapMaybe (maybeDecay notes oct (normalize d r))) m')
        let m' = mappend m s

    returnA -< s


grow :: Double -> Double -> Double
grow r x = r * x * (1 - x)


maybeDecay :: [PitchClass] -> Octave -> Time -> MidiMessage -> Maybe MidiMessage
maybeDecay notes oct dur (Std (NoteOn c k v)) = maybeDecay notes oct dur (ANote c k v dur)
maybeDecay notes oct dur (ANote c k v _)      =
    let (p, _) = pitch c
        ap = absPitch (p, oct)
    in if elem p notes
           then decay (ANote ap k v dur)
           else Nothing


normalize :: Double -> Double -> Double
normalize d r = d * normalizeGrowth r


normalizeGrowth :: Double -> Double
normalizeGrowth x = (/100) $ fromIntegral $ round $ (*100) $ (+0.42) x


notes :: [(String, PitchClass)]
notes = [("C", C), ("Cs", Cs),
         ("D", D), ("Ds", Ds),
         ("E", E),
         ("F", F), ("Fs", Fs),
         ("G", G), ("Gs", Gs),
         ("A", A), ("As", As),
         ("B", B), ("Bs", Bs)]
