
module ArM.Char.Virtues (virtueMap) where

import ArM.Char.Trait
import qualified Data.Map as Map

vl2 :: [ ( TraitKey, VF -> ProtoTrait ) ]
vl2 = [ ( VFKey "Puissant (art)",
         \ x -> defaultPT { art = Just $ vfDetail x, bonusScore = Just 3 } )
     , ( VFKey "Puissant (ability)",
              \ x -> defaultPT { ability = Just $ vfDetail x, bonusScore = Just 2 } )
     , ( VFKey "Affinity with (art)",
              \ x -> defaultPT { art = Just $ vfDetail x, multiplyXP = Just 1.5 } )
     , ( VFKey "Affinity with (ability)",
              \ x -> defaultPT { ability = Just $ vfDetail x, multiplyXP = Just 1.5 } )
     ]


vl1 :: [ ( TraitKey, VF -> ProtoTrait ) ]
vl1 = [ (VFKey ab, \ _ -> defaultPT { ability = Just $ ab, score = Just 1 } ) | ab <- snab ]

snab :: [ String ]
snab = [ "Second Sight", "Enchanting Music", "Dowsing", "Magic Sensitivity" ]

virtueMap :: Map.Map TraitKey ( VF -> ProtoTrait ) 
virtueMap = Map.fromList $ vl1 ++ vl2

