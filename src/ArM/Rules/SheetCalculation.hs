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
import ArM.KeyPair
import ArM.Rules.Aux
import Data.Maybe (fromJust,catMaybes)
import Data.List (sort)

import Control.Parallel.Strategies

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
               . fwdApplyList combatScoreRules 
               . fwdApplyList combatSkillRules
               . fwdApplyList combatRules2
               . fwdApplyList combatRules 
               . addCombatOptions

addCombatOptions :: RDFGraph -> RDFGraph
addCombatOptions g = addGraphs g $ listToRDFGraph g1
    where m = getCOgraph $ getWeapons g
          g1 = fst $ runBlank m ( "combatoptions", 1 )


getWeapons :: RDFGraph -> [ (RDFLabel,RDFLabel) ]
getWeapons = map f . Q.rdfQueryFind q
   where q = listToRDFGraph 
             [ arc cVar (armRes "hasTrait") wVar,
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
                 arc n (armRes "hasCombatWeapon") w:s
combatRules :: [RDFRule]
combatRules = 
    [ makeCRule "combatweaponrule"
      [ arc (Var "sheet") (armRes "hasCombatOption")  cVar
      , arc (Var "sheet") typeRes (armRes "CharacterSheet")
      , arc cVar (armRes "hasWeaponClass") tVar
      , arc (Var "sheet") (armRes "hasWeapon")  (Var "weapon")
      , arc (Var "weapon") typeRes  tVar
      ]
      [ arc cVar (armRes "hasCombatWeapon") (Var "weapon") ]
    , makeCRule "combatshieldrule"
      [ arc (Var "sheet") (armRes "hasCombatOption")  cVar
      , arc (Var "sheet") typeRes (armRes "CharacterSheet")
      , arc cVar (armRes "hasShieldClass") tVar
      , arc (Var "sheet") (armRes "hasWeapon")  (Var "weapon")
      , arc (Var "weapon") typeRes  tVar
      ]
      [ arc cVar (armRes "hasCombatShield") (Var "weapon") ]
    , makeCRule "combatlabel"
      [ arc sVar (armRes "hasCombatOption")  cVar
      , arc cVar (armRes "hasCombatWeapon") tVar
      , arc tVar (armRes "hasLabel") (Var "label")
      ]
      [ arc cVar (armRes "hasLabel") (Var "label") ]
    ]

combatRules2 :: [RDFRule]
combatRules2 = 
    [ makeCRule "combat1rule"
      [ arc (Var "sheet") (armRes "hasCombatOption")  cVar
      , arc (Var "sheet") typeRes (armRes "CharacterSheet")
      , arc cVar (armRes "hasCombatWeapon") (Var "weapon")
      , arc (Var "weapon") (armRes "hasSkill") (Var "skillclass")
      , arc (Var "sheet") (armRes "hasAbility") (Var "skill")
      , arc (Var "skill")  typeRes (Var "skillclass") ]
      [ arc cVar (armRes "hasSkill") (Var "skill") ]
      ]

combatSkillRules :: [RDFRule]
combatSkillRules = 
    [ makeCRule "combat-skillscore-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasSkill") oVar
      , arc oVar  (armRes "hasScore")  (Var "score") ]
      [ arc cVar (armRes "hasSkillScore") (Var "score") ]
    ]

-- | Rules to add relevant constituentstats to each combat option.
-- This is a preparatory step before calculating the actual combat stats. 
combatScoreRules :: [RDFRule]
combatScoreRules =
  [ makeCRule "combat-property-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasCombatWeapon") oVar
      , arc oVar pVar (Var "value")
      , arc pVar typeRes (armRes "WeaponProperty")
      ]
      [ arc cVar pVar (Var "value") ]
  , makeCRule "combat-atk-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasCombatWeapon") oVar
      , arc oVar (armRes "hasWeaponAtk") (Var "score") ]
      [ arc cVar (armRes "hasWeaponAtk") (Var "score") ]
  , makeCRule "combat-dfn-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasCombatWeapon") oVar
      , arc oVar (armRes "hasWeaponDfn") (Var "score") ]
      [ arc cVar (armRes "hasWeaponDfn") (Var "score") ]
  , makeCRule "combat-dfn-shield-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasCombatShield") oVar
      , arc oVar (armRes "hasWeaponDfn") (Var "score") ]
      [ arc cVar (armRes "hasShieldDfn") (Var "score") ]
  , makeCRule "combat-dam-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasCombatWeapon") oVar
      , arc oVar (armRes "hasWeaponDam") (Var "score") ]
      [ arc cVar (armRes "hasWeaponDam") (Var "score") ]
  , makeCRule "combat-init-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasCombatWeapon") oVar
      , arc oVar (armRes "hasWeaponInit") (Var "score") ]
      [ arc cVar (armRes "hasWeaponInit") (Var "score") ]
  , makeCRule "combat-rng-rule"
      [ arc cVar typeRes (armRes "CombatOption")
      , arc cVar (armRes "hasCombatWeapon") oVar
      , arc oVar (armRes "hasWeaponRange") (Var "score") ]
      [ arc cVar (armRes "hasWeaponRange") (Var "score") ]
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



calculateCombatStats :: RDFGraph -> RDFGraph
calculateCombatStats g = foldl addGraphs g fs 
    where fs = parMap rpar f tl
          tl = getCombatStatList g
          f = listToRDFGraph . processCombatStats



getCombatStatList :: RDFGraph -> [[RDFTriple]]
getCombatStatList = arcListSplit . map arcFromBinding . Q.rdfQueryFind q
   where q = listToRDFGraph 
             [ arc idVar typeRes (armRes "CombatOption")
             , arc idVar propertyVar valueVar                 
             , arc propertyVar typeRes (armRes "WeaponProperty")
             ]             

data CombatStats = CombatStats {
    canAtk :: Bool,
    coAtk :: Int, 
    coDfn :: Int,
    coDam :: Int,
    coInit :: Int }
defaultCombatStats :: CombatStats 
defaultCombatStats = CombatStats {
    canAtk = False,
    coAtk = 0,
    coDfn = 0,
    coDam = 0,
    coInit = 0 }
processCombatStats :: [RDFTriple] -> [RDFTriple]
processCombatStats [] = error "No combat stats"
processCombatStats xs = p2 (canAtk cs) 
    where cs = fst $ processCombatStats' (defaultCombatStats,xs)
          co = arcSubj $ head xs
          p1 = arc co (armRes "hasDfn") (litInt $ coDfn cs):
               arc co (armRes "hasInit") (litInt $ coInit cs): []
          p2 False = p1
          p2 True = arc co (armRes "hasAtk") (litInt $ coAtk cs):
                    arc co (armRes "hasDam") (litInt $ coDam cs):p1
processCombatStats' :: (CombatStats,[RDFTriple]) -> (CombatStats,[RDFTriple])
processCombatStats' (cs,[]) = (cs,[])
processCombatStats' (cs,x:xs) 
   | p == armRes "hasWeaponInit" = (cs' { coInit = coInit cs' + v}, xs')
   | p == armRes "hasWeaponInit" = (cs' { coInit = coInit cs' + v}, xs')
   | p == armRes "hasWeaponAtk"  = (cs' { canAtk = True, coAtk = coAtk cs' + v}, xs')
   | p == armRes "hasDex"        = (cs' { coAtk = coAtk cs' + v}, xs')
   | p == armRes "hasWeaponDfn"  = (cs' { coDfn = coDfn cs' + v}, xs')
   | p == armRes "hasShieldDfn"  = (cs' { coDfn = coDfn cs' + v}, xs')
   | p == armRes "hasQik"        = (cs' { coDfn = coDfn cs' + v, coInit = coInit cs' + v} , xs')
   | p == armRes "hasStr"        = (cs' { coDam = coDam cs' + v}, xs' )
   | p == armRes "hasWeaponDam"  = (cs' { coDam = coDam cs' + v}, xs')
   | p == armRes "hasSkillScore" = (cs' { coDfn = coDfn cs' + v, coAtk = coAtk cs' + v} , xs')
   | otherwise                   = (cs', xs')
    where (cs',xs') = processCombatStats' (cs,xs) 
          p = arcPred x
          v = fromJust $ rdfToInt $ arcObj x


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
               . fwdApplyList castingScoreRules

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



-- |
-- = Auxiliaries

-- | Calculate an arc giving a CombatOption score.
-- This is an auxiliary for `addDamInit` and `addAtkDfn`
calc :: String -> RDFLabel -> [Maybe Int] -> RDFTriple 
calc p idvar vb = arc idvar (armRes p) (litInt $ score vb)
    where score xs = foldl (+) 0 $ catMaybes xs
