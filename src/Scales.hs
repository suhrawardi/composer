module Scales (
    intersection,
    otherScales
  ) where

import Data.List (intersect)
import Euterpea
import HSoM


intersection :: Int -> [PitchClass] -> [PitchClass]
intersection scale []          = getNotes scale
intersection scale targetNotes = let scales = getNotes scale
                                 in scales `intersect` targetNotes


getNotes :: Int -> [PitchClass]
getNotes 1 = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
getNotes 2 = [C, Cs,    Ds,       Fs, G,     A, As   ]
getNotes 3 = [C,     D,     E,    Fs, G, Gs,    As   ]
getNotes 4 = [C,     D,     E, F,     G, Gs,        B]
getNotes 5 = [C,     D,     E, F,     G,     A,     B]
getNotes 6 = [C, Cs,        E, F, Fs,    Gs,        B]
getNotes 7 = [C, Cs,    Ds,    F, Fs,    Gs,    As   ]
getNotes 8 = [   Cs, D,     E, F, Fs,    Gs,    As   ]
getNotes 9 = [C, Cs,           F,     G, Gs          ]
getNotes _ = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]


otherScales :: [String]
otherScales = ["None",
               "Mela Ramapriya",
               "Mela Rhisabhapriya",
               "Mela Sarasangi",
               "Mela Kosalam",
               "Raga Bageshri",
               "Persian",
               "Arabic",
               "Arabian",
               "Balinese Pelog"]
