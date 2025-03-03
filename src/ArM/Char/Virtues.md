
module Virtues where

import ArM.Char.Trait
import qualified Data.Map as Map

vl2 = [ ( VFKey "Puissant (art)",
         \ x -> defaultPT { art = detail x, bonusScore = 3 } )
     , ( VFKey "Puissant (ability)",
              \ x -> defaultPT { ability = Just $ detail x, bonusScore = Just 2 } )
     , ( VFKey "Affinity with (art)",
              \ x -> defaultPT { art = Just $ detail x, multiplyXP = Just 1.5 } )
     , ( VFKey "Affinity with (ability)",
              \ x -> defaultPT { ability = Just $ detail x, multiplyXP = Just 1.5 } )
     ]

vl1 = [ (VFKey ab, \ _ -> defaultPT { ability = Just $ ab, score = Just 1 } ) | ab <- snab ]

snab = [ "Second Sight", "Enchanting Music", "Dowsing", "Magic Sensitivity" ]
