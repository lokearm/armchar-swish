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
-- 1.  Infer the subproperties of arm:hasTrait, to make it easy to 
--     extract traits of different kinds (abilities, virtues, etc.)
-- 2.  Calculate Combat Stats.
-- 3.  Calculate casting scores.
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
import Data.Maybe (fromJust)
import Data.List (sort)

import Control.Parallel.Strategies

-- | Prepare a character record graph.
-- This includes merging in the given schema
prepareRecord :: RDFGraph -> RDFGraph -> RDFGraph
prepareRecord schema = addCastingScores . addCombatStats
                 . addScores
                 . fwdApplyList traitRules
                 . fwdApplyList rdfstypeRules
                 . merge schema
                 . fwdApplyList [ traitclasstypeRule ]

-- |
-- = Trait sub properties


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
                     , "Bonus"
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
                     , "CombatOption"
                     , "Bonus"
                     , "OtherTrait"
                     , "Characteristic" ]
    where mkr s = mkr' ("has" ++ s ++ "IRule")
                       (Res $ makeSN s) (Res $ makeSN $ "has" ++ s)
          mkr' s t p = makeCRule s g1 g2 where (g1,g2) = arcs2 t p

arcs1 t p = ( [ arc cVar htRes tVar, arc tVar typeRes t ],
             [ arc cVar p tVar ] ) 
arcs2 t p = ( [ arc cVar p tVar, arc tVar typeRes t ],
             [ arc cVar htRes tVar ] ) 

-- |
-- = Combat stats

-- | Add combat stats to a graph with character sheet data
-- This works in three steps:
-- 1. add necessary traits and weapons to each CombatOption,
-- 2. add the consituent integer scores from each trait and weapon
-- 3. calculate the total combat scores (init/atk/dfn/dam)
addCombatStats = calculateCombatStats
               . fwdApplyListR ( combatScoreRules ++ combatRules )

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
calc p idvar vb = arc idvar (armRes p) (litInt $ score vb)
    where score xs = foldl (+) 0 $ ff xs

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

-- |
-- = Casting Scores
-- 
-- Casting score is the sum of
-- * Technique and Form, each taking the minimum over requisites
-- * Stamina
-- * Possible bonuses
-- * Aura is variable and can be left out here
-- It is further affected by magical foci.

addCastingScores :: RDFGraph -> RDFGraph
addCastingScores = calculateCastingScores
               . addSpellArtScores
               . fwdApplyListR castingScoreRules

-- | These rules add hasFormScore and hasTechScore properties
-- for all the arts used by the spell, including requisites.
-- A further step, not using the rules syntax, is needed to
-- take the minimum over the triples.
castingScoreRules = 
  [ makeCRule "casting-form-rule"
      [ arc sheet (armRes "hasSpell") spell
      , arc spell (armRes "hasForm") art
      , arc sheet (armRes "hasArt") trait
      , arc trait typeRes art
      , arc trait (armRes "hasScore") score ]
      [ arc spell (armRes "hasFormScore") score ]
  , makeCRule "casting-form-req-rule"
      [ arc sheet (armRes "hasSpell") spell
      , arc spell (armRes "hasFormRequisite") art
      , arc sheet (armRes "hasArt") trait
      , arc trait typeRes art
      , arc trait (armRes "hasScore") score ]
      [ arc spell (armRes "hasFormScore") score ]
  , makeCRule "casting-tech-rule"
      [ arc sheet (armRes "hasSpell") spell
      , arc spell (armRes "hasTechnique") art
      , arc sheet (armRes "hasArt") trait
      , arc trait typeRes art
      , arc trait (armRes "hasScore") score ]
      [ arc spell (armRes "hasTechScore") score ]
  , makeCRule "casting-tech-req-rule"
      [ arc sheet (armRes "hasSpell") spell
      , arc spell (armRes "hasTechRequisite") art
      , arc sheet (armRes "hasArt") trait
      , arc trait typeRes art
      , arc trait (armRes "hasScore") score ]
      [ arc spell (armRes "hasTechScore") score ]
  , makeCRule "combat-str-rule"
      [ arc sheet (armRes "hasSpell") spell
      , arc sheet (armRes "hasCharacteristic") trait
      , arc trait typeRes (armrRes "str")
      , arc trait (armRes "hasScore") score ]
      [ arc spell (armRes "hasSta") score ]
  ]
  where spell = Var "spell"
        score = Var "score"
        trait = Var "trait"
        art   = Var "art"
        sheet = Var "cs"

artScores :: String -> RDFGraph -> [RDFTriple]
artScores tp = arcMin . sort . map f . Q.rdfQueryFind q
   where f vb = arc (fromJust $ vbMap vb (Var "spell")) p1 
                    (fromJust $ vbMap vb (Var "score"))
         q = listToRDFGraph 
             [ arc (Var "spell") typeRes (armRes "Spell")
             , arc (Var "spell")  p (Var "score") ]
         p = armRes $ "has" ++ tp ++ "Score"
         p1 = armRes $ "has" ++ tp ++ "EffectiveScore"



arcMin :: [RDFTriple] -> [RDFTriple] 
arcMin [] = []
arcMin (x:[]) = x:[]
arcMin (x:y:xs) | arcSubj x /= arcSubj y = x:arcMin (y:xs)
                | arcPred x /= arcPred y = x:arcMin (y:xs)
                | f x < f y = arcMin (x:xs)
                | otherwise = arcMin (y:xs)
   where f = intFromRDF . arcObj

addSpellArtScores :: RDFGraph -> RDFGraph
addSpellArtScores g = foldl addGraphs g $ map listToRDFGraph xs 
    where xs = parMap rpar ( \ f -> f g ) fs 
          fs = [ artScores "Tech", artScores "Form" ]

calculateCastingScores :: RDFGraph -> RDFGraph
calculateCastingScores g = addGraphs g $ listToRDFGraph 
                         $ map f $ Q.rdfQueryFind q g
   where q = listToRDFGraph 
             [ arc cVar typeRes (armRes "Spell")
             , arc cVar (armRes "hasFormEffectiveScore") (Var "form") 
             , arc cVar (armRes "hasTechEffectiveScore") (Var "tech") 
             , arc cVar (armRes "hasSta") (Var "char") ]
         f vb = calc "hasCastingScore" (fromJust $ vbMap vb cVar) $ ss vb
         ss vb = map vbToInt [ vbMap vb (Var "tech"),
                 vbMap vb (Var "form"), vbMap vb (Var "char") ]

-- !
-- = Scores including Bonuses

addScores = applyRule getEffectiveScores
          . applyRule getBonuses
          . fwdApplyList scoreRules 

-- Note: this needs to be changed to accommodate bonuses.
scoreRules = 
   [ 
   ]


getEffectiveScores :: RDFGraph -> RDFGraph
getEffectiveScores = 
         listToRDFGraph . arcSum "hasScore" . sort . map f . Q.rdfQueryFind q
   where q = listToRDFGraph 
             [ arc cVar typeRes (armRes "Trait")
             , arc cVar (Var "property") (Var "score") 
             , arc (Var "property") typeRes (armRes "ScoreContribution") ]
         f vb = arc (fromJust $ vbMap vb cVar)
                    (fromJust $ vbMap vb (Var "property"))
                    (fromJust $ vbMap vb (Var "score"))

applyRule :: (RDFGraph -> RDFGraph) -> RDFGraph -> RDFGraph
applyRule f g = g `addGraphs` f g

getBonuses :: RDFGraph -> RDFGraph
getBonuses = listToRDFGraph . arcSum "hasTotalBonus" . sort . map f . Q.rdfQueryFind q
   where q = listToRDFGraph 
             [ arc character (armRes "hasTrait") trait
             , arc trait typeRes tVar
             , arc character (armRes "hasBonus") bonus
             , arc bonus (armRes "bonusTo") tVar
             , arc bonus (armRes "hasScore") score ]
         f vb = arc (fromJust $ vbMap vb trait)
                    (fromJust $ vbMap vb bonus)
                    (fromJust $ vbMap vb score)
         character = Var "character"
         score = Var "score"
         bonus = Var "bonus"
         trait = Var "trait"

arcSum :: String -> [RDFTriple] -> [RDFTriple] 
arcSum s [] = []
arcSum s (x:[]) = arc (arcSubj x) (armRes s) (arcObj x):[]
arcSum s (x:y:xs) | arcSubj x /= arcSubj y = x':arcSum s (y:xs)
                | otherwise = arcSum s (y':xs)
   where f = intFromRDF . arcObj
         t = f x + f y
         x' = arc (arcSubj x) (armRes s) (arcObj x)
         y' = arc (arcSubj x) (armRes s) (litInt t)
