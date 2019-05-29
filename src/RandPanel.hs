{-# LANGUAGE Arrows #-}

module RandPanel (
    randPanel
  ) where

import Euterpea
import HSoM
import FRP.UISF
import System.Random


randPanel :: UISF Double Double
randPanel = topDown $ proc dur -> do
    rSeed <- title "Randomness" $ withDisplay (hSlider (2.4, 4.0) 2.4) -< ()
    t <- timer -< dur
    r <- accum 0.1 -< fmap (const (grow rSeed)) t
    returnA -< normalize dur r


grow :: Double -> Double -> Double
grow r x = r * x * (1 - x)


normalize :: Double -> Double -> Double
normalize d r = d * normalizeGrowth r


normalizeGrowth :: Double -> Double
normalizeGrowth x = (/100) $ fromIntegral $ round $ (*100) $ (+0.42) x