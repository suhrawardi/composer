{-# LANGUAGE Arrows #-}

module ChannelPanel (
    channelPanel
  ) where

import Buttons
import Data.Maybe (mapMaybe, isJust)
import DelayPanel
import Euterpea
import HSoM
import FRP.UISF
import RandPanel
import System.Random


channelPanel :: UISF (Int, Maybe [MidiMessage]) (Maybe [MidiMessage])
channelPanel = leftRight $ setSize (560, 600) $ title "Channel" $ proc (channel, miM) -> do
    sourceOcts <- topDown $ setSize (60, 315) $ title "Oct" $ checkGroup octaves -< ()
    sourceNotes <- topDown $ setSize (60, 315) $ title "In" $ checkGroup notes -< ()
    targetNotes <- topDown $ setSize (60, 315) $ title "Out" $ checkGroup notes -< ()

    (isPlaying, miM') <- (| topDown ( do
      (isPlaying) <- (| leftRight ( do
        _ <- title "Channel" display -< channel
        isPlaying <- buttonsPanel -< ()
        returnA -< isPlaying ) |)

      oct <- title "Octave" $ withDisplay (hiSlider 1 (1, 10) 4) -< ()
      delay <- title "Delay" $ withDisplay (hiSlider 1 (0, 50) 0) -< ()

      t <- timer -< 1
      delay' <- randPanel -< (fromIntegral delay, t)
      note <- randNote -< (targetNotes, oct, t)

      _ <- (| leftRight ( do
        _ <- title "Dur" $ display -< delay'
        _ <- title "Rand note" $ display -< note
        returnA -< () ) |)

      rec s <- vdelay -< (delay', fmap (mapMaybe (convert sourceOcts sourceNotes channel oct note)) miM')
          let miM' = mappend miM s
      miM'' <- delayPanel -< (delay', miM')

      returnA -< (isPlaying, miM'') ) |)

    if isPlaying
      then returnA -< miM'
      else returnA -< Nothing


convert :: [Octave] -> [PitchClass] -> Int -> Octave -> Maybe Int -> MidiMessage -> Maybe MidiMessage
convert octs notes channel oct Nothing (Std (NoteOn c k v)) = do
    let (p, o) = pitch c
        randNote = 12 * oct + pcToInt p
    if p `elem` notes && o `elem` octs
        then Just (Std (NoteOn channel randNote v))
        else Nothing
convert octs notes channel oct Nothing (Std (NoteOff c k v)) = do
    let (p, o) = pitch c
        randNote = 12 * oct + pcToInt p
    if p `elem` notes && o `elem` octs
        then Just (Std (NoteOff channel randNote v))
        else Nothing
convert octs notes channel oct note (Std (NoteOn c k v)) = do
    let (p, o) = pitch c
    randN <- note
    if p `elem` notes && o `elem` octs
        then Just (Std (NoteOn channel randN v))
        else Nothing
convert octs notes channel oct note (Std (NoteOff c k v)) = do
    randN <- note
    let (p, o) = pitch c
    if p `elem` notes && o `elem` octs
        then Just (Std (NoteOff channel randN v))
        else Nothing
convert octs notes channel oct note _ = Nothing


randNote :: UISF ([PitchClass], Octave, Maybe ()) (Maybe Int)
randNote = proc (notes, oct, _) -> do
    i <- liftAIO randomRIO -< (0, length notes - 1)
    let note = case notes of
                    []  -> Nothing
                    _   -> Just $ absPitch (notes !! i, oct)
    returnA -< note


notes :: [(String, PitchClass)]
notes = [("C", C), ("Cs", Cs),
         ("D", D), ("Ds", Ds),
         ("E", E),
         ("F", F), ("Fs", Fs),
         ("G", G), ("Gs", Gs),
         ("A", A), ("As", As),
         ("B", B), ("Bs", Bs)]


octaves :: [(String, Octave)]
octaves = [("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5),
           ("6", 6), ("7", 7), ("8", 8), ("9", 9)]


sGen :: StdGen
sGen = mkStdGen 42
