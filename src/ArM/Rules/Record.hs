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
-- 3. calculate the total combat scores (init/atk/def/dam)
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
  , makeCRule "combat-def-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeapon") oVar
      , arc oVar (armRes "hasWeaponDef") (Var "score") ]
      [ arc cVar (armRes "hasWeaponDef") (Var "score") ]
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

initQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeaponInit") (Var "weapon") 
      , arc cVar (armRes "hasQik") (Var "char") ]
atkQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasSkillScore") (Var "skill") 
      , arc cVar (armRes "hasWeaponAtk") (Var "weapon") 
      , arc cVar (armRes "hasDex") (Var "char") ]
defQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasSkillScore") (Var "skill") 
      , arc cVar (armRes "hasWeaponDef") (Var "weapon") 
      , arc cVar (armRes "hasQik") (Var "char") ]
damQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeaponDam") (Var "weapon") 
      , arc cVar (armRes "hasStr") (Var "char") ]

addAtkDef :: RDFGraph -> RDFGraph -> [RDFTriple]
addAtkDef q = map f . Q.rdfQueryFind q
           where f vb = arc cVar (armRes "hasDam") (litInt $ score vb)
                 score vb = foldl (+) 0 $ ff $ ss vb
                 ss vb = map vbToInt [ vbMap vb (Var "weapon"),
                        vbMap vb (Var "score") ,
                        vbMap vb (Var "char") ]
addDamInit :: RDFGraph -> RDFGraph -> [RDFTriple]
addDamInit q = map f . Q.rdfQueryFind q
           where f vb = arc cVar (armRes "hasDam") (litInt $ score vb)
                 score vb = foldl (+) 0 $ ff $ ss vb
                 ss vb = map vbToInt [ vbMap vb (Var "weapon"),
                        vbMap vb (Var "char") ]
addfunctions = [ addDamInit damQuery
               , addDamInit initQuery
               , addAtkDef atkQuery
               , addAtkDef defQuery ]
calculateCombatStats g = foldl addGraphs g $ map listToRDFGraph fs 
    where fs = parMap rpar ( \ f -> f g ) addfunctions 

ff :: [Maybe Int] -> [Int]
ff [] = [] 
ff (Nothing:xs) = ff xs
ff (Just x:xs) = x:ff xs
vbToInt :: Maybe RDFLabel -> Maybe Int
vbToInt Nothing = Nothing
vbToInt (Just x) = fromRDFLabel x
