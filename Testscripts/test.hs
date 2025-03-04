
import ArM.Char.Virtues
import ArM.Char.Trait
import qualified Data.Map as Map
import Data.Maybe

k = VFKey "Puissant (art)"


-- ghci> :t lookup
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- ghci> % :t Map.lookup
-- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a

v = VF "Puissant (art)" "Creo" 5 Nothing

-- ghci> vv
-- VF {vfname = "Puissant (art)", vfDetail = "Creo", vfcost = 5, vfAppliesTo = Nothing}

f = fromJust (Map.lookup k virtueMap )

-- ghci> f vv
-- Art: Creo 0xp

