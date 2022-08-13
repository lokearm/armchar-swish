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
--
-----------------------------------------------------------------------------

module ArM.Rules.Record (prepareRecord) where

import qualified Swish.RDF.Query as Q
import Swish.VarBinding  (vbMap)
import Swish.RDF.Graph
import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Vocabulary.XSD
import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.Common
import ArM.Rules.RDFS
import Debug.Trace
import Data.Maybe (fromJust)

import Control.Parallel.Strategies

-- | Prepare a character record graph.
-- This includes merging in the given schema
prepareRecord schema = addCombatStats
                 . fwdApplyList traitRules
                 . fwdApplyList rdfstypeRules
                 . merge schema
                 . fwdApplyList [ traitclasstypeRule ]
-- | Add combat stats to a graph with character sheet data
-- This works in three steps:
-- 1. add necessary traits and weapons to each CombatOption,
-- 2. add the consituent integer scores from each trait and weapon
-- 3. calculate the total combat scores (init/atk/dfn/dam)
addCombatStats = calculateCombatStats
               . fwdApplyListR ( combatScoreRules ++ combatRules )

traitRules = traitRules1 ++ traitRules2
-- | Rules to infer subproperties of arm:hasTrait
traitRules1 = map mkr [ "Ability"
                     , "Virtue"
                     , "Flaw"
                     , "PersonalityTrait"
                     , "Reputation"
                     , "Spell"
                     , "Art"
                     , "CombatOption"
                     , "OtherTrait"
                     , "Characteristic" ]
    where mkr s = mkr' ("has" ++ s ++ "Rule")
                       (Res $ makeSN s) (Res $ makeSN $ "has" ++ s)
          mkr' s t p = makeCRule s g1 g2 where (g1,g2) = arcs1 t p
-- | Rules to infer arm:hasTrait from subproperties
traitRules2 = map mkr [ "Ability"
                     , "Virtue"
                     , "Flaw"
                     , "PersonalityTrait"
                     , "Reputation"
                     , "Spell"
                     , "Art"
                     , "OtherTrait"
                     , "Characteristic" ]
    where mkr s = mkr' ("has" ++ s ++ "IRule")
                       (Res $ makeSN s) (Res $ makeSN $ "has" ++ s)
          mkr' s t p = makeCRule s g1 g2 where (g1,g2) = arcs2 t p

arcs1 t p = ( [ arc cVar htRes tVar, arc tVar typeRes t ],
             [ arc cVar p tVar ] ) 
arcs2 t p = ( [ arc cVar p tVar, arc tVar typeRes t ],
             [ arc cVar htRes tVar ] ) 

combatRules = 
    [ makeCRule "combat1rule"
      [ arc sVar (armRes "hasCombatOption")  cVar
      , arc sVar typeRes (armRes "CharacterSheet")
      , arc cVar (armRes "weaponClass") tVar
      , arc sVar (armRes "hasPossession") oVar
      , arc oVar typeRes tVar ]
      [ arc cVar (armRes "hasWeapon") oVar ]
-- 1.
-- CharacterSheet has CombatOption
-- CombatOption has WeaponClass
-- Weapon is WeaponClass
-- CharacterSheet has Weapon
-- => CombatOption has Weapon
    , makeCRule "combat2rule"
      [ arc sVar (armRes "hasCombatOption")  cVar
      , arc sVar typeRes (armRes "CharacterSheet")
      , arc cVar (armRes "skillClass") tVar
      , arc sVar (armRes "hasTrait") oVar
      , arc oVar typeRes tVar ]
      [ arc cVar (armRes "hasSkill") oVar ]
-- Ability analogous to Weapon (possession) above
    ]

-- | Rules to add relevant constituentstats to each combat option.
-- This is a preparatory step before calculating the actual combat stats. 
combatScoreRules =
  [ makeCRule "combat-property-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeapon") oVar
      , arc oVar pVar (Var "value")
      , arc pVar typeRes (armRes "WeaponProperty")
      ]
      [ arc cVar pVar (Var "value") ]
  , makeCRule "combat-skillscore-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasSkill") oVar
      , arc oVar (armRes "hasScore") (Var "score") ]
      [ arc cVar (armRes "hasSkillScore") (Var "score") ]
  , makeCRule "combat-atk-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeapon") oVar
      , arc oVar (armRes "hasWeaponAtk") (Var "score") ]
      [ arc cVar (armRes "hasWeaponAtk") (Var "score") ]
  , makeCRule "combat-dfn-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeapon") oVar
      , arc oVar (armRes "hasWeaponDfn") (Var "score") ]
      [ arc cVar (armRes "hasWeaponDfn") (Var "score") ]
  , makeCRule "combat-dam-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeapon") oVar
      , arc oVar (armRes "hasWeaponDam") (Var "score") ]
      [ arc cVar (armRes "hasWeaponDam") (Var "score") ]
  , makeCRule "combat-init-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeapon") oVar
      , arc oVar (armRes "hasWeaponInit") (Var "score") ]
      [ arc cVar (armRes "hasWeaponInit") (Var "score") ]
  , makeCRule "combat-qik-rule"
      [ arc sVar (armRes "hasCombatOption")  cVar
      , arc sVar (armRes "hasCharacteristic")  tVar
      , arc tVar typeRes (armrRes "qik")
      , arc tVar (armRes "hasScore") (Var "score") ]
      [ arc cVar (armRes "hasQik") (Var "score") ]
  , makeCRule "combat-dex-rule"
      [ arc sVar (armRes "hasCombatOption")  cVar
      , arc sVar (armRes "hasCharacteristic")  tVar
      , arc tVar typeRes (armrRes "dex")
      , arc tVar (armRes "hasScore") (Var "score") ]
      [ arc cVar (armRes "hasDex") (Var "score") ]
  , makeCRule "combat-str-rule"
      [ arc sVar (armRes "hasCombatOption")  cVar
      , arc sVar (armRes "hasCharacteristic")  tVar
      , arc tVar typeRes (armrRes "str")
      , arc tVar (armRes "hasScore") (Var "score") ]
      [ arc cVar (armRes "hasStr") (Var "score") ]
  ]

--
-- E.g. Atk
-- CombatOption has Skill, Skill has Score
-- CombatOption has Weapon, Skill hasAtk Score
-- Character has CombatOption, Character has Dex, Dex has  Score
-- ADD all the scores
-- => CombatOption hasAtk Score

-- 3. (foreach)
-- Character has Weapon
-- Weapon has Skill
-- => Character has NEW CombatOption
-- CombatOption has Skill
-- NB. New blank nodes.

-- | Query to get constituent scores for Init Score
initQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeaponInit") (Var "weapon") 
      , arc cVar (armRes "hasQik") (Var "char") ]
-- | Query to get constituent scores for Attack Score
atkQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasSkillScore") (Var "skill") 
      , arc cVar (armRes "hasWeaponAtk") (Var "weapon") 
      , arc cVar (armRes "hasDex") (Var "char") ]
-- | Query to get constituent scores for Defence Score
dfnQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasSkillScore") (Var "skill") 
      , arc cVar (armRes "hasWeaponDfn") (Var "weapon") 
      , arc cVar (armRes "hasQik") (Var "char") ]
-- | Query to get constituent scores for Damage Score
damQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeaponDam") (Var "weapon") 
      , arc cVar (armRes "hasStr") (Var "char") ]

-- | Function constructing Attack/Defence Scores.
addAtkDfn :: String -- ^ Either "hasAtk" or "hasDfn"
      -> RDFGraph -- ^ Either `atkQuery` or `dfnQuery`
      -> RDFGraph -- ^ The graph of character data
      -> [RDFTriple] -- ^ List of new triples; should be one per CombatOption
addAtkDfn p q = map f . Q.rdfQueryFind q
           where f vb = calc p (fromJust $ vbMap vb cVar) $ ss vb
                 ss vb = map vbToInt [ vbMap vb (Var "weapon"),
                        vbMap vb (Var "skill") ,
                        vbMap vb (Var "char") ]
-- | Function constructing Init/Damage Scores.
addDamInit :: String -- ^ Either "hasInit" or "hasDam"
      -> RDFGraph -- ^ Either `initQuery` or `damQuery`
      -> RDFGraph -- ^ The graph of character data
      -> [RDFTriple] -- ^ List of new triples; should be one per CombatOption
addDamInit p q = map f . Q.rdfQueryFind q
           where f vb = calc p (fromJust $ vbMap vb cVar) $ ss vb
                 ss vb = map vbToInt [ vbMap vb (Var "weapon"),
                        vbMap vb (Var "char") ]
-- | Calculate an arc giving a CombatOption score.
-- This is an auxiliary for `addDamInit` and `addAtkDfn`
calc :: String -> RDFLabel -> [Maybe Int] -> RDFTriple 
calc p idvar vb = trace ("Arc: " ++ show a) a
    where score xs = foldl (+) 0 $ ff xs
          a = arc idvar (armRes p) (litInt $ score vb)

addfunctions = [ addDamInit "hasInit" damQuery
               , addDamInit "hasDam"  initQuery
               , addAtkDfn  "hasAtk"  atkQuery
               , addAtkDfn  "hasDfn"  dfnQuery ]
calculateCombatStats g = foldl addGraphs g $ map listToRDFGraph fs 
    where fs = parMap rpar ( \ f -> f g ) addfunctions 

ff :: [Maybe Int] -> [Int]
ff [] = [] 
ff (Nothing:xs) = ff xs
ff (Just x:xs) = x:ff xs
vbToInt :: Maybe RDFLabel -> Maybe Int
vbToInt Nothing = Nothing
vbToInt (Just x) = Just $ intFromRDF x

rdfToInt :: RDFLabel -> Maybe Int
rdfToInt = fromRDFLabel
rdfToString :: RDFLabel -> Maybe String
rdfToString = fromRDFLabel

intFromRDF :: RDFLabel -> Int
intFromRDF x = fi i
   where i = rdfToInt x
         s = rdfToString x
         fi Nothing = fs s
         fi (Just y) = y
         fs (Just y) = read y
