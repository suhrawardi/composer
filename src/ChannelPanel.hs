{-# LANGUAGE Arrows #-}

module ChannelPanel (
    channelPanel
  ) where

import Buttons
import Data.Maybe (mapMaybe, isJust)
import Euterpea
import HSoM
import FRP.UISF
import System.Random


channelPanel :: UISF (Int, Maybe [MidiMessage]) (Maybe [MidiMessage])
channelPanel = topDown $ setSize (500, 600) $ proc (channel, miM) -> do
    _ <- title "Channel" display -< channel
    isPlaying <- buttonsPanel >>> handleButtons -< ()
    oct <- title "Octave" $ withDisplay (hiSlider 1 (1, 10) 4) -< ()

    sourceNotes <- topDown $ setSize (60, 315) $ title "In" $ checkGroup notes -< ()
    targetNotes <- topDown $ setSize (60, 315) $ title "Out" $ checkGroup notes -< ()
    t <- timer -< 1
    note <- randNote -< (targetNotes, oct, t)

    -- rec s <- vdelay -< (0.1, fmap (mapMaybe (convert channel randNote)) miM')
    --     let miM' = mappend miM s

    if isPlaying
      then returnA -< fmap (mapMaybe (convert sourceNotes channel oct note)) miM
      else returnA -< Nothing


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
