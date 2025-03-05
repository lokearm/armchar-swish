-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Record
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoning rules applied to the CharacterRecord graph, using
-- the schema ontology.
--
-- The following functionality is applied
-- 1.  Calculate ability scores and XP towards next level.
-- 2.  Infer the subproperties of arm:hasTrait, to make it easy to 
--     extract traits of different kinds (abilities, virtues, etc.)
-- 3.  Calculate effective scores (including bonuses like puissant)
-- 3.  Calculate Combat Stats.
-- 4.  Calculate casting scores.
--
-----------------------------------------------------------------------------

module ArM.Rules.Record (prepareRecord) where

import qualified Swish.RDF.Query as Q
import Swish.VarBinding  (vbMap)
import Swish.RDF.Graph
-- import Swish.RDF.Vocabulary.RDF
-- import Swish.RDF.Vocabulary.XSD
import Swish.RDF.Ruleset (RDFRule)
import ArM.GameRules
import ArM.Swish.Resources
import ArM.Rules.Aux
import ArM.Rules.Common
import ArM.Rules.RDFS
import Data.Maybe (fromJust)
-- import Data.List (sort)

-- import Control.Parallel.Strategies

-- | Prepare a character record graph.
-- This includes merging in the given schema
prepareRecord :: RDFGraph -> RDFGraph -> RDFGraph
prepareRecord schema = fwdApplyList traitRules
                 . processXP
                 . fwdApplyList rdfstypeRules
                 . merge schema
                 . fwdApplyList classRules

-- |
-- = Trait sub properties

traitTypeStrings :: [String]
traitTypeStrings = [ "Ability"
                     , "Virtue"
                     , "Flaw"
                     , "PersonalityTrait"
                     , "Reputation"
                     , "Spell"
                     , "Art"
                     , "CombatOption"
                     , "Bonus"
                     , "OtherTrait"
                     , "Characteristic" 
                     , "Equipment" 
                     , "Book" 
                     , "Vis" 
                     , "Weapon" 
                     ]

traitRules :: [RDFRule]
traitRules = traitRules1 
-- | Rules to infer subproperties of arm:hasTrait
traitRules1 :: [RDFRule]
traitRules1 = map mkr traitTypeStrings
    where mkr s = mkr' ("has" ++ s ++ "Rule")
                       (Res $ makeSN s) (Res $ makeSN $ "has" ++ s)
          mkr' s t p = makeCRule s g1 g2 where (g1,g2) = arcs1 t p

arcs1 :: RDFLabel -> RDFLabel -> ([RDFTriple],[RDFTriple])
arcs1 t p = ( [ arc cVar htRes tVar, arc tVar typeRes t ],
             [ arc cVar p tVar ] ) 

-- |
-- = Recalculate Scores and XP

-- | Calculate score and remaining XP from total XP.
processXP :: RDFGraph -> RDFGraph
processXP g = foldr addArc g' g1 
    where g1 = processArtXP g
          g2 = processAbXP g
          g' = foldr addArc g g2

-- | Calculate the triples for total XP, score, and remaining XP,
-- given an XPType object.
-- This is an auxiliary for `processXP`
processXP' :: String -> Int -> RDFGraph -> [RDFTriple]
processXP' s n g = foldr (++) [] $ map ( makeXPArcs n . f ) $ Q.rdfQueryFind q g
   where f vb = (fromJust $ vbMap vb (Var "trait")
                , fromJust $ vbToInt $ vbMap vb (Var "xp"))
         q = listToRDFGraph
             [ arc ( Var "trait" ) ( armRes "hasTotalXP" ) ( Var "xp" )
             , arc ( Var "trait" ) typeRes ( armRes s ) ]

-- Note the use of `foldr`, which is a lot faster than `foldl`.

processArtXP :: RDFGraph -> [RDFTriple]
processArtXP = processXP' "XPTrait" 5
processAbXP :: RDFGraph -> [RDFTriple]
processAbXP = processXP' "AccelleratedTrait" 1
          

makeXPArcs :: Int -> (RDFLabel,Int) -> [RDFTriple]
makeXPArcs n (trait,total) = [ arc trait (armRes "hasXPScore") (litInt score),
                             arc trait (armRes "hasXP") (litInt xp) ]
   where score = scoreFromXP (total `div` n)
         xp = total - n*(score*(score+1) `div` 2)
