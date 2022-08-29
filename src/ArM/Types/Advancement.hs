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
module ArM.Types.Advancement where

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe
import Data.List (sort)
import ArM.Types.Season
import ArM.KeyPair
import ArM.Resources
-- import ArM.BlankNode
import ArM.Rules.Aux
import ArM.Types.RDF
import ArM.Types.Trait
import qualified ArM.Types.Season as TS
import Data.Aeson
import Data.Aeson.Key
import qualified Swish.RDF.VarBinding  as VB
import           Swish.VarBinding  (vbMap)

import ArM.Trace

-- |
-- = Character Advancement

-- | CharacterAdvancement Resource
-- Essential information is in `rdfid`, `contents`, and `traits.
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
   makeRDFGraph cs =  listToRDFGraph  ( advToArcList cs ) 

advToArcList :: Advancement -> [RDFTriple]
advToArcList adv = ys2
    where ms = keyvalueToArcList (rdfid adv) (contents adv)
          xs1 =  map traitContents (traits adv)
          xs2 =  map traitContents (items adv)
          ys1 = foldr (++) ms xs1
          ys2 = foldr (++) ys1 xs2


-- |
-- = JSON

data ProtoAdvancement = ProtoAdvancement {
    advancementchar :: RDFLabel,
    advancementid :: RDFLabel,
    advancementcontents :: KeyPairList,
    advancementtraits :: [Trait],
    advancementitems :: [Trait]
   } 

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
   parseJSON (Object v) = ProtoAdvancement <$> v .: "advancementid"
                                           <*> v .: "advancementcontents"
                                           <*> v .: "advancementtraits"
                                           <*> v .: "advancementitems"
                                           <*> v .: "advancementcharacter"
   parseJSON _ = error "Non-exhaustive pattern when parsing ProtoAdvancement from JSON."
fromProtoAdvancement :: ProtoAdvancement -> Advancement
fromProtoAdvancement adv = defaultAdvancement 
                     { rdfid = advancementid adv
                     , traits = advancementtraits adv
                     , items = advancementitems adv
                     , advChar = advancementchar adv
                     , advTime = parseTime TS.defaultCharTime ys
                     , contents = ys
                 } where ys = fromKeyPairList $ advancementcontents adv

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

instance FromRDFGraph Advancement where
   fromRDFGraph g label = fixAdv g $ defaultAdvancement 
                 { rdfid = label
                 , advTime = parseTime defaultCharTime ys 
                 , contents = ys }
        where q = listToRDFGraph  $
                  [ arc label typeRes (armRes "CharacterAdvancement"),
                    arc label (Var "property") (Var "value"),
                    arc (Var "property") typeRes (armRes "ViewProperty"),
                    arc (Var "property") labelRes (Var "label") ]
              vb = Q.rdfQueryFind g q
              ys = map keypairFromBinding vb

-- | Auxiliary for 'fixAdvancements'
fixAdv :: RDFGraph -> Advancement -> Advancement
fixAdv g adv = trace ("fixAdv "++show advid) $ adv { traits = traitsFromRDF advid g,
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
    | traitClass t == c = splitTrait' (t':ts,xs) 
    | otherwise         = splitTrait' (mkTrait x:t:ts,xs) 
       where t' = addToTrait t x
             (c,_,_,_) = x
mkTrait :: ProtoTrait -> Trait
mkTrait (a,b,c,d) = defaultTrait { traitClass = a,
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
             = t { instanceLabel = lab o
                 , traitContents = triple:traitContents t }
      | p == typeRes && o == armRes "RepeatableTrait" 
                        = t { isRepeatableTrait = True
                            , traitContents = triple:traitContents t }
      | p == typeRes && o == armRes "Equipment" 
                        = t { isRepeatableTrait = True
                            , traitContents = triple:traitContents t }
      | otherwise = t { traitContents = triple:traitContents t }
         where lab = f . rdfToString 
               triple = arc s p o
               f Nothing = ""
               f (Just x) = x

