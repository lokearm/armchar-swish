module ArM.Types.Character where

import Data.Set (fromList)
import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe
import ArM.KeyPair
import ArM.Resources
import ArM.BlankNode
import ArM.Rules.Aux

-- ** Trait ** 

-- | Trait Resource
-- `traitID` and `traitContents` are sufficient to describe the trait.
-- The other fields duplicate information to facilitate searching and
-- sorting.
-- When new traits are created, `traitID` is set to nothing?
-- A blank node is only created when it is written into an RDFGraph.
data Trait = Trait {
    traitID :: Maybe RDFLabel,
    traitClass :: RDFLabel,
    isRepeatableTrait :: Bool,
    isXPTrait :: Bool,
    isAccelleratedTrait :: Bool,
    traitContents :: [KeyValuePair]
   } deriving (Eq)
defaultTrait = Trait {
    traitID = Nothing,
    traitClass = noSuchTrait,
    isRepeatableTrait = False,
    isXPTrait = False,
    isAccelleratedTrait = False,
    traitContents = []
   } 

instance Show Trait where
   show a = "**" ++ y (traitID a) ++ " " ++ show (traitClass a) ++ "**\n" 
                 ++ sc (traitContents a) 
                 ++ "\n"
      where 
         y Nothing = ""
         y (Just x) = show x
         s Nothing = ""
         s (Just x) = x
         sc [] = ""
         sc (KeyValuePair x y:xs) = "  " ++ show x ++ ": " ++ show y ++ "\n" ++ sc xs
instance Ord Trait where
   compare x y | traitClass x < traitClass y = LT
               | traitClass x > traitClass y = GT
               | traitID x < traitID y = LT
               | traitID x > traitID y = GT
               | otherwise = EQ


keyvalueToArcList :: RDFLabel -> [KeyValuePair] -> [RDFTriple]
keyvalueToArcList x [] = []
keyvalueToArcList x (KeyValuePair a c:ys) = arc x a c:keyvalueToArcList x ys

traitToArcListM :: RDFLabel -> Trait -> BlankState [RDFTriple]
traitToArcListM cs t 
     | x' == Nothing = do
                      y <- getBlank 
                      return $ arc cs htRes y:keyvalueToArcList y ts
     | otherwise    = return $ arc cs htRes x:keyvalueToArcList x ts
                 where x' = traitID t
                       x = fromJust x'
                       ts = traitContents t


data CharacterSheet = CharacterSheet {
         csID :: RDFLabel,
         sheetID :: Maybe RDFLabel,
         csYear :: Maybe Int,
         csSeason :: String,
         -- csSeason :: Maybe RDFLabel,
         csTraits :: [Trait],
         csMetadata :: [KeyValuePair]
       }  deriving (Eq)
instance Show CharacterSheet where
    show cs = "**" ++ show (csID cs) ++ "**\n" 
           ++ "-- " ++ ( showSheetID ) cs ++ "\n"
           ++ "Traits:\n" ++ showw ( csTraits cs )
           ++ "Metadata Triples:\n" ++ showw ( csMetadata cs )
        where showw [] = ""
              showw (x:xs) = "  " ++ show x ++ "\n" ++ showw xs
defaultCS = CharacterSheet {
         csID = noSuchCharacter,
         sheetID = Nothing,
         csYear = Nothing,
         csSeason = "",
         csTraits = [],
         csMetadata = []
       }  

showSheetID :: CharacterSheet -> String
showSheetID = f . sheetID
    where f Nothing = "no sheet ID"
          f (Just x) = show x

class FromRDFGraph a where
    fromRDFGraph :: RDFGraph -> RDFLabel -> a 
class ToRDFGraph a where
    makeRDFGraph :: a -> RDFGraph
instance ToRDFGraph CharacterSheet where
   makeRDFGraph cs =
         ( toRDFGraph .  fromList . fst . runBlank ( csToArcListM cs ) )
         ("charsheet",1)


csToArcListM :: CharacterSheet -> BlankState [RDFTriple]
csToArcListM cs = do
          x <- getSheetIDM cs $ sheetID cs
          ts <- mapM (traitToArcListM x) (csTraits cs)
          let ms = keyvalueToArcList x (csMetadata cs)
          let ct = arc x isCharacterLabel charlabel
          let ct1 = arc x typeRes csRes 
          return $ ct1:ct:foldl (++) ms ts
    where 
          charlabel = csID cs

getSheetIDM :: CharacterSheet -> Maybe RDFLabel -> BlankState RDFLabel
getSheetIDM _ Nothing = getBlank
getSheetIDM _ (Just x) = return x

-- | CharacterAdvancement Resource
-- Essential information is in `rdfid`, `contents`, and `traits.
-- The other properties are redundant, merely giving access to
-- critical information without having to search the lists.
-- TraitAdvancements are represented as a list of `Trait`s.
-- Other properties are listed as 'contents'.
data Advancement = Advancement {
    year :: Maybe Int,
    season :: String,
    -- season :: Maybe RDFLabel,
    rdfid :: RDFLabel,
    contents :: [KeyValuePair],
    advSortIndex :: Int,
    traits :: [Trait]
   } deriving Eq

defaultAdvancement = Advancement { year = Nothing,
                season = "", rdfid = noSuchAdvancement, 
                advSortIndex = 0,
                contents = [], traits = [] }
instance Show Advancement where
   show a = show (rdfid a) ++ "\n  **" ++ (season a) ++ " " ++ y (year a) ++ "**\n" 
                 ++ sc (contents a) 
                 ++ show (traits a) 
                 ++ "\n"
      where 
         y Nothing = ""
         y (Just x) = show x
         sc [] = ""
         sc (KeyValuePair x y:xs) = show x ++ ": " ++ show y ++ "\n" ++ sc xs
         st [] = ""
         st ((x,_,y,z):xs) = "  " ++ show x ++ ": " ++ y ++ " - " ++ z 
                                  ++  "\n" ++ st xs
instance Ord Advancement where
   compare x y | advSortIndex x < advSortIndex y = LT
               | advSortIndex x > advSortIndex y = GT
               | year x < year y = LT
               | year x > year y = GT
               | sno x < sno y = LT
               | sno x > sno y = GT
               | rdfid x < rdfid y = LT
               | rdfid x > rdfid y = GT
               | contents x < contents y = LT
               | contents x > contents y = GT
               | otherwise = EQ
               where sno = seasonNo . season

seasonNo "Spring" = 1
seasonNo "Summer" = 2
seasonNo "Autumn" = 3
seasonNo "Winter" = 4
seasonNo _ = 10
-- seasonNo Nothing = 0
-- seasonNo (Just x ) | x == springLabel = 1
--                    | x == summerLabel = 2
--                    | x == autumnLabel = 3
--                    | x == winterLabel = 4
--                    | otherwise  = 10

instance ToRDFGraph Advancement where
   makeRDFGraph cs =
         ( toRDFGraph .  fromList . fst . runBlank ( advToArcListM cs ) )
         ("charsheet",1)

advToArcListM :: Advancement -> BlankState [RDFTriple]
advToArcListM adv = mapM (traitToArcListM x) (traits adv) 
                >>= return . foldl (++) ms 
    where ms = keyvalueToArcList x (contents adv)
          x = rdfid adv
