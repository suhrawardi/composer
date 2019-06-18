{-# LANGUAGE Arrows #-}

module ChannelPanel (
    channelPanel
  ) where

import Buttons
import Data.Maybe (mapMaybe, isJust)
import Debugger
import DelayPanel
import Euterpea
import HSoM
import FRP.UISF
import RandPanel
import Scales
import System.Random
import Tuning


channelPanel :: UISF (Int, Int, Maybe [MidiMessage]) (Maybe [MidiMessage])
channelPanel = leftRight $ setSize (560, 718) $ title "Channel" $ proc (channel, tuning, miM) -> do
    sourceOcts <- topDown $ setSize (60, 696) $ title "Oct" $ checkGroup octaves -< ()
    sourceNotes <- topDown $ setSize (60, 696) $ title "In" $ checkGroup notes -< ()

    (isPlaying, isLearning, tick, moM) <- (| topDown ( do
      scale <- title "Other tuning" $ radio otherScales 0 -< ()
      (isPlaying, isLearning) <- (| leftRight ( do
        _ <- title "Channel" display -< channel
        (isPlaying, isLearning) <- buttonsPanel -< ()
        returnA -< (isPlaying, isLearning) ) |)

      oct <- title "Octave" $ withDisplay (hiSlider 1 (2, 7) 4) -< ()
      delay <- title "Delay" $ withDisplay (hiSlider 1 (0, 50) 0) -< ()

      f <- title "Tempo" $ withDisplay (hSlider (1, 10) 2) -< ()
      tick <- timer -< 1/f

      delay' <- randPanel -< (fromIntegral delay, tick)

      _ <- (| leftRight ( do
        _ <- title "Dur" $ display -< delay'
        returnA -< () ) |)

      rec s <- vdelay -< (delay', fmap (mapMaybe (convert sourceOcts notes channel oct)) miM)
          let moM = mappend Nothing s
              notes = intersection scale sourceNotes
      moM' <- delayPanel -< (delay', moM)

      returnA -< (isPlaying, isLearning, tick, moM') ) |)

    if isLearning
      then
        returnA -< fmap (const [ANote channel 36 100 01]) tick
      else
        if isPlaying
          then do
            moM' <- adjustTuning -< (tuning, maybeTrace(moM))
            returnA -< moM'
          else
            returnA -< Nothing


convert :: [Octave] -> [PitchClass] -> Int -> Octave -> MidiMessage -> Maybe MidiMessage
convert octs notes channel oct (Std (NoteOn c k v)) = do
    let (p, o) = pitch k
        note = 12 * (oct + 1) + pcToInt p
    if p `elem` notes && o `elem` octs
        then Just (Std (NoteOn channel note v))
        else Nothing
convert octs notes channel oct (Std (ControlChange c 2 k)) = do
    let (p, o) = pitch k
        note = 12 * (oct + 1) + pcToInt p
    if p `elem` notes && o `elem` octs
        then Just (ANote channel note 127 (1/3))
        else Nothing
convert octs notes channel oct (Std (NoteOff c k v)) = do
    let (p, o) = pitch k
        note = 12 * (oct + 1) + pcToInt p
    if p `elem` notes && o `elem` octs
        then Just (Std (NoteOff channel note v))
        else Nothing
convert octs notes channel oct _ = Nothing


notes :: [(String, PitchClass)]
notes = [("C", C), ("Cs", Cs),
         ("D", D), ("Ds", Ds),
         ("E", E),
         ("F", F), ("Fs", Fs),
         ("G", G), ("Gs", Gs),
         ("A", A), ("As", As),
         ("B", B)]


octaves :: [(String, Octave)]
octaves = [("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5),
           ("6", 6), ("7", 7), ("8", 8), ("9", 9), ("10", 10)]


sGen :: StdGen
sGen = mkStdGen 42
