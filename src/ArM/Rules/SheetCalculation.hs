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

module ArM.Rules.SheetCalculation (calculateSheet) where

import qualified Swish.RDF.Query as Q
import Swish.VarBinding  (vbMap)
import Swish.RDF.Graph
-- import Swish.RDF.Vocabulary.RDF
-- import Swish.RDF.Vocabulary.XSD
import Swish.RDF.Ruleset (RDFRule)
import ArM.Resources
import ArM.Rules.Aux
import Data.Maybe (fromJust)
import Data.List (sort)

import Control.Parallel.Strategies
import ArM.Debug.Trace

import ArM.BlankNode

-- | Prepare a character record graph.
-- This includes merging in the given schema
calculateSheet :: RDFGraph -> RDFGraph
calculateSheet = addCastingScores . addCombatStats . addScores

-- |
-- = Combat stats

-- | Add combat stats to a graph with character sheet data
-- This works in three steps:
-- 1. add necessary traits and weapons to each CombatOption,
-- 2. add the consituent integer scores from each trait and weapon
-- 3. calculate the total combat scores (init/atk/dfn/dam)
addCombatStats :: RDFGraph -> RDFGraph
addCombatStats = calculateCombatStats
               . fwdApplyListR combatScoreRules 
               . addDefaultSkill
               . fwdApplyListR ( combatRules ++ combatSkillRules)
               . addCombatOptions

addCombatOptions :: RDFGraph -> RDFGraph
addCombatOptions g = addGraphs g $ listToRDFGraph g1
    where m = getCOgraph $ getWeapons g
          g1 = fst $ runBlank m ( "combatoptions", 1 )


getWeapons :: RDFGraph -> [ (RDFLabel,RDFLabel) ]
getWeapons = map f . Q.rdfQueryFind q
   where q = listToRDFGraph 
             [ arc cVar (armRes "hasTraitlike") wVar,
               arc wVar typeRes (armRes "GeneralWeapon") ]
         f vb = (fromJust $ vbMap vb cVar, fromJust $ vbMap vb wVar)
         wVar = Var "weapon"
getCOgraph :: [ (RDFLabel,RDFLabel) ] -> BlankState [RDFTriple]
getCOgraph [] = return []
getCOgraph ((c,w):xs) = do
           n <- getBlank
           s <- getCOgraph xs
           return $
                 arc c (armRes "hasCombatOption") n:
                 arc n typeRes (armRes "CombatOption"):
                 arc n (armRes "hasWeapon") w:s

combatRules :: [RDFRule]
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
      , arc sVar htRes oVar
      , arc oVar typeRes tVar ]
      [ arc cVar (armRes "hasSkill") oVar ]
-- Ability analogous to Weapon (possession) above
    , makeCRule "combatlabel"
      [ arc sVar (armRes "hasCombatOption")  cVar
      , arc cVar (armRes "hasWeapon") tVar
      , arc tVar (armRes "hasLabel") (Var "label")
      ]
      [ arc cVar (armRes "hasLabel") (Var "label") ]
    ]

combatSkillRules :: [RDFRule]
combatSkillRules = 
    [ makeCRule "combat-skillscore-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasSkill") oVar
      , arc oVar  (armRes "hasEffectiveScore")  (Var "score") ]
      [ arc cVar (armRes "hasSkillScore") (Var "score") ]
    ]
--     makeCRule "combatskill"
--       [ arc sVar (armRes "hasCombatOption")  cVar
--       , arc cVar (armRes "hasSkill") (Var "skill")
--       ]
--       [ arc sVar (armRes "hasSkill") (Var "skill") ]
--    ]

-- | Rules to add relevant constituentstats to each combat option.
-- This is a preparatory step before calculating the actual combat stats. 
combatScoreRules :: [RDFRule]
combatScoreRules =
  [ makeCRule "combat-property-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeapon") oVar
      , arc oVar pVar (Var "value")
      , arc pVar typeRes (armRes "WeaponProperty")
      ]
      [ arc cVar pVar (Var "value") ]
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
initQuery :: RDFGraph
initQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasWeaponInit") (Var "weapon") 
      , arc cVar (armRes "hasQik") (Var "char") ]
-- | Query to get constituent scores for Attack Score
atkQuery :: RDFGraph
atkQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasSkillScore") (Var "skill") 
      , arc cVar (armRes "hasWeaponAtk") (Var "weapon") 
      , arc cVar (armRes "hasDex") (Var "char") ]
-- | Query to get constituent scores for Defence Score
dfnQuery :: RDFGraph
dfnQuery = listToRDFGraph 
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasSkillScore") (Var "skill") 
      , arc cVar (armRes "hasWeaponDfn") (Var "weapon") 
      , arc cVar (armRes "hasQik") (Var "char") ]
-- | Query to get constituent scores for Damage Score
damQuery :: RDFGraph
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

addfunctions :: [ RDFGraph -> [RDFTriple] ]
addfunctions = [ addDamInit "hasDam" damQuery
               , addDamInit "hasInit"  initQuery
               , addAtkDfn  "hasAtk"  atkQuery
               , addAtkDfn  "hasDfn"  dfnQuery ]
calculateCombatStats :: RDFGraph -> RDFGraph
calculateCombatStats g = foldl addGraphs g $ map listToRDFGraph fs 
    where fs = parMap rpar ( \ f -> f g ) addfunctions 

ff :: [Maybe Int] -> [Int]
ff [] = [] 
ff (Nothing:xs) = ff xs
ff (Just x:xs) = x:ff xs


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
castingScoreRules :: [RDFRule] 
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

addScores :: RDFGraph -> RDFGraph
addScores = applyRule getEffectiveScores
          . applyRule getBonuses

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
             [ arc character htRes trait
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

-- | Add RDFTriples for each subject in a sorted list of triples.
-- For each subject a new triple is created by adding the objects
-- which are assumed to be integers in the original triples.
arcSum :: String     -- ^ String identifying the property for the new triple
          -> [RDFTriple]  -- ^ Input list
          -> [RDFTriple]  -- ^ Output list
arcSum _ [] = []
arcSum s (x:[]) = arc (arcSubj x) (armRes s) (arcObj x):[]
arcSum s (x:y:xs) | arcSubj x /= arcSubj y = x':arcSum s (y:xs)
                | otherwise = arcSum s (y':xs)
   where f = intFromRDF . arcObj
         t = f x + f y
         x' = arc (arcSubj x) (armRes s) (arcObj x)
         y' = arc (arcSubj x) (armRes s) (litInt t)


-- | Add zero skills
addDefaultSkill :: RDFGraph -> RDFGraph
addDefaultSkill g = addGraphs g $ g1 g
   where g1 = listToRDFGraph . map f . ttrace . defaultSkillPairs 
         f (l,i) = arc l (armRes "hasSkillScore") (litInt i)

defaultSkillPairs :: RDFGraph -> [ (RDFLabel, Int) ]
defaultSkillPairs g = f cs co
    where
        co = sort $ getCombatOptions g
        cs = sort $ getCombatOptionSkills g
        f (x:xs) (y:ys) | fst x < y = trace ("scored" ++ show (fst x)) $ f xs (y:ys)
                        | fst x > y = trace ("default" ++ show y) $ (y,0):f (x:xs) (ys)
                        | otherwise = trace ("equal" ++ show y) $ f xs ys
        f _ []  = []
        f [] ys  = [ (y,0) | y <- ys ]
getCombatOptions :: RDFGraph -> [ RDFLabel ]
getCombatOptions = map f . Q.rdfQueryFind q
   where q = listToRDFGraph 
             [ arc coVar typeRes (armRes "CombatOption") ]
         f vb = (fromJust $ vbMap vb coVar)
         coVar = Var "co"
getCombatOptionSkills :: RDFGraph -> [ (RDFLabel, Int) ]
getCombatOptionSkills = map f . Q.rdfQueryFind q
   where q = listToRDFGraph 
             [ arc coVar typeRes (armRes "CombatOption") 
             , arc coVar (armRes "hasSkillScore")  scoreVar ]
         f vb = ( (fromJust $ vbMap vb coVar),
                  intFromRDF (fromJust $ vbMap vb scoreVar) )
         coVar = Var "co"
         scoreVar = Var "score"
