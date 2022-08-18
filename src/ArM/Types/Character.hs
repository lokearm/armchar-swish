module ArM.Types.Character where

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

traitToArcListM :: TraitLike a => RDFLabel -> RDFLabel -> a -> BlankState [RDFTriple]
traitToArcListM hasTraitRes cs t 
     | x' == Nothing = do
                      y <- getBlank 
                      return $ arc cs hasTraitRes y:keyvalueToArcList y ts
     | otherwise    = return $ arc cs hasTraitRes x:keyvalueToArcList x ts
                 where x' = traitlikeID t
                       x = fromJust x'
                       ts = traitlikeContents t

class TraitLike a where
    traitlikeID :: a -> Maybe RDFLabel
    traitlikeClass :: a -> RDFLabel
    traitlikeContents :: a -> [KeyValuePair]
instance TraitLike Trait where
    traitlikeID = traitID
    traitlikeClass = traitClass
    traitlikeContents = traitContents
instance TraitLike Item where
    traitlikeID = itemID
    traitlikeClass = itemClass
    traitlikeContents = itemContents


data Item = Item {
    itemID :: Maybe RDFLabel,
    itemClass :: RDFLabel,
    itemLabel :: String,
    itemContents :: [KeyValuePair]
   } deriving (Eq)
defaultItem = Item {
    itemID = Nothing,
    itemClass = noSuchTrait,
    itemLabel = "",
    itemContents = []
   } 
instance Show Item where
   show a = "**" ++ itemLabel a ++ "(" ++ y (itemID a) ++ ") "
                 ++ show (itemClass a) ++ "**\n" 
                 ++ sc (itemContents a) 
                 ++ "\n"
      where 
         y Nothing = ""
         y (Just x) = show x
         s Nothing = ""
         s (Just x) = x
         sc [] = ""
         sc (KeyValuePair x y:xs) = "  " ++ show x ++ ": " ++ show y ++ "\n" ++ sc xs
instance Ord Item where
   compare x y | itemClass x < itemClass y = LT
               | itemClass x > itemClass y = GT
               | itemLabel x < itemLabel y = LT
               | itemLabel x > itemLabel y = GT
               | itemID x < itemID y = LT
               | itemID x > itemID y = GT
               | otherwise = EQ

data Character = Character {
         characterID :: RDFLabel,
         characterData :: KeyPairList
       }  deriving (Eq)
instance Show Character where
    show cs = "**" ++ show (characterID cs) ++ "**\n" 
           ++ "Metadata Triples:\n" ++ show ( characterData cs )
        where showw [] = ""
              showw (x:xs) = "  " ++ show x ++ "\n" ++ showw xs
defaultCharacter = Character {
         characterID = noSuchCharacter,
         characterData = KeyPairList []
       }  

data CharacterSheet = CharacterSheet {
      csID :: RDFLabel,
      -- ^ Character ID (i.e. same ID for every season)
      sheetID :: Maybe RDFLabel,  
      -- ^ ID of the Character Sheet, usually Nothing suggesting a blank node
      csYear :: Maybe Int,  -- ^ Current Year
      csSeason :: String,   -- ^ Current Season
      born     :: Int,      -- ^ Year of Birth
      csItems :: [Item],    -- ^ List of possessions (weapons, equipment)
      csTraits :: [Trait],  -- ^ List of traits (abilities, spells, etc.)
      csMetadata :: KeyPairList
      -- ^ Metadata, i.e. data which are not traits or items.
      }  deriving (Eq)
instance Show CharacterSheet where
    show cs = "**" ++ show (csID cs) ++ "**\n" 
           ++ "-- " ++ ( showSheetID ) cs ++ "\n"
           ++ "Traits:\n" ++ showw ( csTraits cs )
           ++ "Metadata Triples:\n" ++ show ( csMetadata cs )
        where showw [] = ""
              showw (x:xs) = "  " ++ show x ++ "\n" ++ showw xs
defaultCS = CharacterSheet {
         csID = noSuchCharacter,
         sheetID = Nothing,
         csYear = Nothing,
         csSeason = "",
         born = 0,
         csItems = [],
         csTraits = [],
         csMetadata = KeyPairList []
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
         ( listToRDFGraph  . fst . runBlank ( csToArcListM cs' ) )
         ("charsheet",1)
      where cs' = cs { csMetadata = KeyPairList $ a xs }
            KeyPairList xs = csMetadata cs
            aAge age ys  
                   | age == 0 = ys
                   | otherwise = KeyValuePair (armRes "hasAge") (litInt age):ys
            aYear Nothing ys   = ys
            aYear (Just x) ys  = KeyValuePair (armRes "inYear") (litInt x):ys
            aSeason x ys  
                   | x == "" = ys
                   | otherwise = KeyValuePair (armRes "atSeason") (litString x):ys
            age' Nothing _ = 0
            age' (Just y) b | b == 0 = 0 
                            | otherwise = y - b
            age = age' (csYear cs) (born cs)
            a = aAge age . aYear (csYear cs) . aSeason (csSeason cs)
instance ToRDFGraph Character where
   makeRDFGraph cs = listToRDFGraph  ( ct:ms )
       where x = characterID cs
             ms = keyvalueToArcList x (fromKeyPairList $ characterData cs)
             ct = arc x typeRes (armRes  "Character")
csToArcListM :: CharacterSheet -> BlankState [RDFTriple]
csToArcListM cs = do
          x <- getSheetIDM cs $ sheetID cs
          ts <- mapM (traitToArcListM htRes x) (csTraits cs)
          is <- mapM (traitToArcListM (armRes "hasPossession") x) (csItems cs)
          let ms = keyvalueToArcList x (fromKeyPairList $ csMetadata cs)
          let ct = arc x isCharacterLabel (csID cs)
          let ct1 = arc x typeRes csRes 
          let ms1 = foldr (++) ms ts
          return $ ct1:ct:foldr (++) ms1 is

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
    rdfid :: RDFLabel,
    contents :: [KeyValuePair],
    advSortIndex :: Int,
    traits :: [Trait],
    items :: [Item]
   } deriving Eq

defaultAdvancement = Advancement { year = Nothing,
                season = "",
                rdfid = noSuchAdvancement, 
                advSortIndex = 0,
                contents = [],
                traits = [],
                items = []
                }
instance Show Advancement where
   show a = show (rdfid a) ++ "\n  **" ++ (season a) ++ " " ++ y (year a) ++ "**\n" 
                 ++ sc (contents a) 
                 ++ show (traits a) 
                 ++ show (items a) 
                 ++ "\nSort Index: " ++ show (advSortIndex a) 
                 ++ "\nSeason No: " ++ show (sno a) 
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
sno = seasonNo . season

-- | The `SeasonYear` gives a season with year.
type SeasonYear = (String,Int) 
-- | Given a season/year, `nextSeason` returns the subsequent season.
-- Winter is the last season of the year, and is followed by the Spring of
-- the next year.
nextSeason :: SeasonYear -> SeasonYear
nextSeason ("Spring",y) = ("Summer",y)
nextSeason ("Summer",y) = ("Autumn",y)
nextSeason ("Autumn",y) = ("Winter",y)
nextSeason ("Winter",y) = ("Spring",y+1)


-- | Given a season as a String, `nextSeason` returns a number by which 
-- seasons can be ordered within a calendar year.
-- Winter is the last season in the year.
seasonNo :: String -> Int
seasonNo "Spring" = 1
seasonNo "Summer" = 2
seasonNo "Autumn" = 3
seasonNo "Winter" = 4
seasonNo _ = 10

instance ToRDFGraph Advancement where
   makeRDFGraph cs =
         ( listToRDFGraph  . fst . runBlank ( advToArcListM cs ) )
         ("charsheet",1)

advToArcListM :: Advancement -> BlankState [RDFTriple]
advToArcListM adv = do
        xs1 <- mapM (traitToArcListM atRes x) (traits adv) 
        xs2 <- mapM (traitToArcListM cpRes x) (items adv) 
        let ys1 = foldr (++) ms xs1
        let ys2 = foldr (++) ys1 xs2
        return ys2
    where ms = keyvalueToArcList x (contents adv)
          x = rdfid adv
          atRes = armRes "advanceTrait"
          cpRes = armRes "changePossession"
