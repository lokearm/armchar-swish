{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle Advancement
--
-----------------------------------------------------------------------------
module ArM.Types.Advancement ( Advancement(..)
                             , getPregameAdvancements
                             , getIngameAdvancements
                             , getAllAdvancements
                             ) where

import ArM.Debug.Trace

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe
import Data.List (sort)
import ArM.Types.Season
import ArM.KeyPair
import ArM.Resources
import ArM.BlankNode
import ArM.Rules.Aux
import ArM.Types.RDF
import ArM.Types.Trait
import qualified ArM.Types.Season as TS
import Data.Aeson
import Data.Aeson.Key
import qualified Swish.RDF.VarBinding  as VB
import           Swish.VarBinding  (vbMap)

-- |
-- = Character Advancement

-- | Character Advancement Resource
-- Essential information is in `rdfid`, `contents`, and `traits`.
-- The other properties are redundant, merely giving access to
-- critical information without having to search the lists.
-- TraitAdvancements are represented as a list of `Trait`s.
-- Other properties are listed as 'contents'.
data Advancement = Advancement 
    { advChar :: RDFLabel
    , advTime :: CharTime
    , rdfid :: RDFLabel
    , contents :: [KeyValuePair]
    , traits :: [Trait]
    , items :: [Trait]
   } deriving Eq

advSortIndex :: Advancement -> Int
advSortIndex = advancementIndex . advTime
year :: Advancement -> Int
year = f . hasYear 
   where f Nothing = 0
         f (Just y) = y
season :: Advancement -> String
season = charSeason . advTime

defaultAdvancement :: Advancement 
defaultAdvancement = Advancement 
                { advChar = armRes "noSuchCharacter"
                , rdfid = noSuchAdvancement
                , contents = []
                , advTime = defaultCharTime
                , traits = []
                , items = []
                }
instance Show Advancement where
   show a = show (rdfid a) ++ "\n  **" ++ (season a) ++ " " ++ show (year a) ++ "**\n" 
                 ++ sc (contents a) 
                 ++ show (traits a) 
                 ++ show (items a) 
                 ++ "\nSort Index: " ++ show (advSortIndex a) 
                 ++ "\nSeason No: " ++ show (sno a) 
                 ++ "\n"
      where 
         sc [] = ""
         sc (KeyValuePair x y:xs) = show x ++ ": " ++ show y ++ "\n" ++ sc xs

instance HasTime Advancement where
    timeOf = advTime
instance Ord Advancement where
   compare x y | advTime x < advTime y = LT
               | advTime x > advTime y = GT
               | rdfid x < rdfid y = LT
               | rdfid x > rdfid y = GT
               | contents x < contents y = LT
               | contents x > contents y = GT
               | otherwise = EQ

sno :: Advancement -> Int
sno = seasonNo . season

instance ToRDFGraph Advancement where
   makeRDFGraph cs =  
         ( listToRDFGraph  . fst . runBlank ( advToArcListM cs ) )
         ("advancement",1)

advToArcListM :: Advancement -> BlankState [RDFTriple]
advToArcListM adv = do
       tsm <- fixBlanksM $ traits adv
       ism <- fixBlanksM $ items adv
       let x = rdfid adv
       let xs1 =  map traitContents tsm
       let xs2 =  map traitContents ism
       let ht = map ( \ y -> arc x (armRes "advanceTrait") (traitID y) ) tsm
       let hi = map ( \ y -> arc x (armRes "changePossession") (traitID y) ) ism
       let ys1 = foldr (++) (ms++hi++ht) xs1
       return $ foldr (++) ys1 xs2
    where ms = keyvalueToArcList (rdfid adv) (contents adv)


-- |
-- = JSON

data ProtoAdvancement = ProtoAdvancement {
    advancementcharacter :: RDFLabel,
    advancementid :: RDFLabel,
    advancementcontents :: KeyPairList,
    advancementtraits :: [Trait],
    advancementitems :: [Trait]
   } deriving Show

instance ToJSON Advancement where 
    toJSON cs = object (c:s:x:z:y:[])
       where x = (fromString "advancementtraits") .= (toJSON (traits cs))
             z = (fromString "advancementitems") .= (toJSON (items cs))
             y = (fromString "advancementcontents") .= KeyPairList (contents cs)
             c = (fromString "advancementid") .= toJSON (rdfid cs)
             s = (fromString "advancementcharacter") .= toJSON (advChar cs)

instance FromJSON Advancement where 
   parseJSON = fmap fromProtoAdvancement . parseJSON
instance FromJSON ProtoAdvancement where 
   parseJSON (Object v) = ProtoAdvancement <$> v .: "advancementcharacter"
                                           <*> v .: "advancementid"
                                           <*> v .: "advancementcontents"
                                           <*> v .: "advancementtraits"
                                           <*> v .: "advancementitems"
   -- NOTE.  The ordering of the fields in parseJSON above has to
   -- match the ordering in the Algebraic Datatyep.
   parseJSON _ = error "Non-exhaustive pattern when parsing ProtoAdvancement from JSON."
fromProtoAdvancement :: ProtoAdvancement -> Advancement
fromProtoAdvancement adv = defaultAdvancement 
                     { rdfid = advancementid adv
                     , traits = advancementtraits adv
                     , items = advancementitems adv
                     , advChar = advancementcharacter adv
                     , advTime = tm
                     , contents = ys
                 } where ys = fromKeyPairList $ advancementcontents adv
                         tm = parseTime TS.defaultCharTime ys

parseTime :: CharTime  -> [KeyValuePair] -> CharTime
parseTime a [] = a
parseTime ain (xin:xs) = parseTime (f ain xin) xs
  where f a (KeyValuePair k v) 
         | k == inYear = a { charYear = rdfToInt v }
         | k == atSeason = a { charSeason = fs (rdfToString v) }
         | k == hasAdvancementIndex = a { advancementIndex = fi (rdfToInt v) }
         | otherwise = a
         where fs Nothing = "" 
               fs (Just x) = x
               fi Nothing = 0 
               fi (Just x) = x

-- | Auxiliary for 'fixAdvancements'
fixAdv :: RDFGraph -> Advancement -> Advancement
fixAdv g adv = adv { traits = traitsFromRDF advid g,
                 items = itemsFromRDF advid g }
        where advid = rdfid adv

itemsFromRDF :: RDFLabel -> RDFGraph -> [Trait]
itemsFromRDF advid g = itFromRDF True "changePossession" advid g
traitsFromRDF :: RDFLabel -> RDFGraph -> [Trait]
traitsFromRDF advid g = itFromRDF False "advanceTrait" advid g

itFromRDF :: Bool -> String -> RDFLabel -> RDFGraph -> [Trait]
itFromRDF b s advid g = splitTrait $ sort $ map (vb2tt b) $ Q.rdfQueryFind q g 
    where q = traitqgraph (armRes s) advid

type ProtoTrait = (RDFLabel, RDFLabel, RDFLabel,RDFLabel)
vb2tt :: Bool -> VB.RDFVarBinding -> ProtoTrait
vb2tt _ vb = ( fromJust $ vbMap vb (Var "class")
             , (fromJust $ vbMap vb (Var "id")) 
             , (fromJust $ vbMap vb (Var "property"))
             , (fromJust $ vbMap vb (Var "value")) 
             )

splitTrait :: [ProtoTrait] -> [Trait]
splitTrait xs = fst $ splitTrait' ([],xs)
splitTrait' :: ([Trait],[ProtoTrait]) -> ([Trait],[ProtoTrait])
splitTrait' (ts,[]) = (ts,[])
splitTrait' ([],x:xs) = splitTrait' (mkTrait x:[],xs) 
splitTrait' (t:ts,x:xs) 
    | traitID t == id = splitTrait' (t':ts,xs) 
    | otherwise       = trace (show t) $ splitTrait' (mkTrait x:t:ts,xs) 
       where t' = addToTrait t x
             (_,id,_,_) = x
mkTrait :: ProtoTrait -> Trait
mkTrait (a,b,c,d) = defaultTrait { traitClass = a,
			 traitID = b,
                         traitContents = [ arc b c d ] }

traitqgraph :: RDFLabel -> RDFLabel -> RDFGraph
traitqgraph p s = listToRDFGraph 
      [ arc s p (Var "id")
      , arc (Var "id") (Var "property") (Var "value")
      , arc (Var "id") (armRes "traitClass") (Var "class") ]

addToTrait :: Trait -> ProtoTrait -> Trait
addToTrait t (c,s,p,o) 
      | traitClass t /= c = error "traitClass mismatch in addToTrait"
      | p == armRes "instanceLabel" 
             = trace "> instanceLabel" $ t { instanceLabel = ttrace $ lab o
                 , traitContents = triple:traitContents t }
      | p == typeRes && o == armRes "RepeatableTrait" 
                        = trace "> repeatable" $ t { isRepeatableTrait = True
                            , traitContents = triple:traitContents t }
      | p == typeRes && o == armRes "Equipment" 
                        = trace "> Equipment" $ t { isRepeatableTrait = True
                            , traitContents = triple:traitContents t }
      | otherwise = t { traitContents = triple:traitContents t }
         where lab = f . rdfToString 
               triple = ttrace $ arc s p o
               f Nothing = ""
               f (Just x) = x

-- | Get a list of all Pregame Advancements of a character.
getPregameAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getPregameAdvancements g c = getAdvancements g $ queryGraph preGameAdv c
   where preGameAdv = armRes  "PregameAdvancement"

-- | Get a list of all Ingame Advancements of a character.
getIngameAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getIngameAdvancements g c = getAdvancements g $ queryGraph inGameAdv c
   where inGameAdv = armRes  "IngameAdvancement"

getAllAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getAllAdvancements g c = getAdvancements g $ queryGraph inGameAdv c
   where inGameAdv = armRes  "CharacterAdvancement"
-- getAllAdvancements g c = map ( \ x -> x { advChar = c } ) 
          -- $ getAdvancements g $ listToRDFGraph  
          -- [ arc (Var "id") (Var "property") (Var "value")
          -- , arc (Var "id") (armRes  "advanceCharacter") c
          -- , arc (Var "property") labelRes (Var "label") ]

-- | Query graph to find a advancements of a given type (RDF class)
-- for a given character.
queryGraph :: RDFLabel -- ^ Label for the advancement type
           -> RDFLabel -- ^ Label for the character to be advanced
           -> RDFGraph -- ^ Resulting graph
queryGraph c = listToRDFGraph  . f c
   where f c1 c2 = [ arc (Var "id") typeRes c1,
            arc (Var "id") (Var "property") (Var "value"),
            arc (Var "id") (armRes  "advanceCharacter") c2,
            arc (Var "property") labelRes (Var "label") ]

-- | Generic version of 'getIngameAdvancements' and 'getPregameAdvancements'
getAdvancements :: RDFGraph -> RDFGraph -> [Advancement]
getAdvancements g = fixAdvancements g . 
               map toAdvancement . arcListSplit . getGenQuads g 

-- | Auxiliary for 'getAdvancements'
getGenQuads :: RDFGraph -> RDFGraph -> [RDFTriple]
getGenQuads g q = map arcFromBinding $ Q.rdfQueryFind q g

-- | Auxiliary for `getAdvancements`
fixAdvancements :: RDFGraph -> [Advancement] -> [Advancement]
fixAdvancements g adv = map (fixAdv g) adv

-- | Make an Advancement object from a list of Quads
toAdvancement :: [RDFTriple] -> Advancement
toAdvancement xs = defaultAdvancement 
                 { rdfid = getkey xs
                 , advChar = fm $ getProperty (armRes "advanceCharacter") ys 
                 , advTime = parseTime defaultCharTime ys 
                 , contents = ys }
         where ys = toKeyPairList xs 
               getkey [] = noSuchAdvancement
               getkey (x:_) = arcSubj x
               fm Nothing = armRes "noSuchCharacter"
               fm (Just x) = x

