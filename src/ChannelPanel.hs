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
channelPanel = leftRight $ setSize (500, 600) $ title "Channel" $ proc (channel, miM) -> do
    sourceNotes <- topDown $ setSize (60, 315) $ title "In" $ checkGroup notes -< ()
    targetNotes <- topDown $ setSize (60, 315) $ title "Out" $ checkGroup notes -< ()

    (isPlaying, oct, delay, note) <- (| topDown ( do
      (isPlaying) <- (| leftRight ( do
        _ <- title "Channel" display -< channel
        isPlaying <- buttonsPanel >>> handleButtons -< ()
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

      returnA -< (isPlaying, oct, delay', note) ) |)

    if isPlaying
      then do
        rec s <- vdelay -< (delay, fmap (mapMaybe (convert sourceNotes channel oct note)) miM')
            let miM' = mappend miM s
        miM'' <- delayPanel -< (delay, miM')
        returnA -< miM''
      else do
        returnA -< Nothing


convert :: [PitchClass] -> Int -> Octave -> Maybe Int -> MidiMessage -> Maybe MidiMessage
convert notes channel oct Nothing (Std (NoteOn c k v)) = do
    let (p, _) = pitch c
        randNote = 12 * oct + pcToInt p
    if p `elem` notes
        then Just (Std (NoteOn channel randNote v))
        else Nothing
convert notes channel oct Nothing (Std (NoteOff c k v)) = do
    let (p, _) = pitch c
        randNote = 12 * oct + pcToInt p
    if p `elem` notes
        then Just (Std (NoteOff channel randNote v))
        else Nothing
convert notes channel oct note (Std (NoteOn c k v)) = do
    let (p, _) = pitch c
    randN <- note
    if p `elem` notes
        then Just (Std (NoteOn channel randN v))
        else Nothing
convert notes channel oct note (Std (NoteOff c k v)) = do
    randN <- note
    let (p, _) = pitch c
    if p `elem` notes
        then Just (Std (NoteOff channel randN v))
        else Nothing
convert notes channel oct note _ = Nothing


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


sGen :: StdGen
sGen = mkStdGen 42
