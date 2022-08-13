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
prepareRecord schema = fwdApplyList combatRules 
                 . fwdApplyList rdfstypeRules
                 . fwdApplyList rdfstypeRules
                 . merge schema
                 . fwdApplyList [ traitclasstypeRule ]

traitRules = traitRules1 ++ traitRules2
-- | Rules to infer subproperties of arm:hasTrait
traitRules1 = map mkr [ "Ability"
                     , "Virtue"
                     , "Flaw"
                     , "PersonalityTrait"
                     , "Reputation"
                     , "Spell"
                     , "Art"
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
      , arc sVar (armRes "hasWeapon") oVar
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
  [ makeCRule "combat-skillscore-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasTrait") oVar
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
