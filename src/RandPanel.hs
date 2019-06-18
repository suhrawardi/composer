{-# LANGUAGE Arrows #-}

module RandPanel (
    randPanel,
    randNote
  ) where

import Euterpea
import HSoM
import FRP.UISF
import System.Random


randPanel :: UISF (Double, Maybe ()) Double
randPanel = leftRight $ title "Randomness" $ proc (dur, delay) -> do
    rSeed <- hSlider (2.4, 4.0) 2.4 -< ()
    r <- accum 0.1 -< fmap (const (grow rSeed)) delay
    let delay = normalize dur r
    _ <- display -< delay
    returnA -< delay


randNote :: UISF ([PitchClass], Octave, Maybe ()) (Maybe Int)
randNote = proc (notes, oct, _) -> do
    i <- liftAIO randomRIO -< (0, length notes - 1)
    let note = case notes of
                    []  -> Nothing
                    _   -> Just $ absPitch (notes !! i, oct)
    returnA -< note


grow :: Double -> Double -> Double
grow r x = r * x * (1 - x)


normalize :: Double -> Double -> Double
normalize d r = d * normalizeGrowth r


normalizeGrowth :: Double -> Double
normalizeGrowth x = (/100) $ fromIntegral $ round $ (*100) $ (+0.42) x
