{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Types to Traits and advancement of Traits
--
-- Traits are represented as `ProtoTrait` objects in the design and
-- advancement. A `ProtoTrait` can represent either a new trait or a
-- change to an existing trait.
--
-- Processing advancements, the `ProtoTrait` is converted to a `Trait` 
-- object, representing the current state of the trait.
--
-- There are individual types for each kind of trait, e.g. virtue/flaw,
-- ability, spell, art, etc.   Conversion between a `Trait` object and
-- a more specific type, like `Art`, is done by the (polymorphic)
-- `toTrait` and `getTrait` functions.
--
-----------------------------------------------------------------------------
module ArM.Char.Trait ( ProtoTrait(..)
                      , Trait(..)
                      , TraitKey(..)
                      , Characteristic(..)
                      , OtherTrait(..)
                      , Confidence(..)
                      , Ability(..)
                      , Art(..)
                      , PTrait(..)
                      , Spell(..)
                      , Reputation(..)
                      , VF(..)
                      , sortTraits
                      , TraitLike(..)
                      , TraitType(..)
                      , advance
                      , defaultPT
                      , spellTeFoLe
                      , spellKeyName
                       ) where

import ArM.GameRules
import ArM.Helper

import GHC.Generics
import Data.Aeson
import Data.Maybe (fromJust,isNothing,fromMaybe)
import Data.List (sortBy)

import ArM.Debug.Trace

-- | 
-- = ProtoTrait

-- | A `ProtoTrait` represents a new trait or an advancement of an existing trait
-- as represented in the JSON input.  
-- Most fields are used only for certain types of traits and are Nothing for other
-- types.  This is the case, in particular, for the first quite a few fields which
-- give the name of the trait of the relevant type only. 
data ProtoTrait = ProtoTrait
    { ability :: Maybe String  -- ^ ability name 
    , virtue :: Maybe String   -- ^ virtue name
    , flaw :: Maybe String     -- ^ flaw name
    , characteristic :: Maybe String  -- ^ characteristic name
    , art :: Maybe String  -- ^ art name
    , spell :: Maybe String  -- ^ spell name
    , ptrait :: Maybe String  -- ^ personality trait name
    , confidence :: Maybe String  -- ^ confidence, true faith, or similar
    , reputation :: Maybe String  -- ^ reputation contents
    , other :: Maybe String       -- ^ other trait, e.g. warping or decrepitude
    , spec :: Maybe String        -- ^ specialisation of an ability
    , detail :: Maybe String      -- ^ detail (options) for a virtue or flaw
    , appliesTo :: Maybe TraitKey  -- ^ not used (intended for virtues/flaws applying to another trait)
    , level :: Maybe Int       -- ^ level of a space
    , tefo :: Maybe String     -- ^ technique/form of a spell
    , locale :: Maybe String   -- ^ locale/domain of a reputation
    , mastery :: Maybe [ String ]   -- ^ mastery options for a spell
    , score :: Maybe Int       -- ^ new score to replace the old one
    , bonusScore :: Maybe Int  -- ^ bonus from puissant; should also be used on initial 
                               -- characteristics for the bonus from Strong Faerie Blood
                               -- or Great/Poor Characteristic to keep it separate from
                               -- the levels bought from points.
    , multiplyXP :: Maybe Float  -- ^ XP multiplier from affinities and similar
    , cost :: Maybe Int          -- ^ cost of a virtue or flaw
    , points :: Maybe Int        -- ^ points for confidence/true faith/etc (additive)
    , xp :: Maybe XPType         -- ^ XP to be added to the trait
    , aging :: Maybe Int         -- ^ aging points for characteristicds (additive)
    , multiplicity :: Maybe Int  -- ^ number of types a virtue/flaw is taken;
                                 -- could be negative to remove an existing, but
                                 -- this is not yet implemented
    , comment :: Maybe String    -- ^ freeform comment
    } deriving (Ord,Eq,Generic)

-- | Default ProtoTrait object, used internally for step-by-step construction of
-- new objects.
defaultPT :: ProtoTrait
defaultPT = ProtoTrait { ability = Nothing
                             , virtue = Nothing
                             , flaw = Nothing
                             , characteristic = Nothing
                             , art = Nothing
                             , spell = Nothing
                             , ptrait = Nothing
                             , confidence = Nothing
                             , reputation = Nothing
                             , other = Nothing
                             , spec = Nothing
                             , detail = Nothing
                             , appliesTo = Nothing
                             , level = Nothing
                             , tefo = Nothing
                             , locale = Nothing
                             , mastery = Nothing
                             , score = Nothing
                             , bonusScore = Nothing
                             , multiplyXP = Nothing
                             , cost = Nothing
                             , points = Nothing
                             , xp = Nothing
                             , aging = Nothing
                             , multiplicity = Nothing
                             , comment = Nothing
                             }

instance ToJSON ProtoTrait 
instance FromJSON ProtoTrait where
    parseJSON = withObject "ProtoTrait" $ \v -> ProtoTrait
        <$> v .:?  "ability"
        <*> v .:?  "virtue"
        <*> v .:?  "flaw"
        <*> v .:?  "characteristic"
        <*> v .:?  "art"
        <*> v .:?  "spell"
        <*> v .:?  "ptrait"
        <*> v .:?  "confidence"
        <*> v .:?  "reputation"
        <*> v .:?  "other"
        <*> v .:?  "spec"
        <*> v .:?  "detail"
        <*> v .:?  "appliesTo"
        <*> v .:?  "level"
        <*> v .:?  "tefo"
        <*> v .:?  "locale"
        <*> v .:?  "mastery"
        <*> v .:?  "score"
        <*> v .:?  "bonusScore"
        <*> v .:?  "multiplyXP"
        <*> v .:?  "cost"
        <*> v .:?  "points"
        <*> v .:?  "xp"
        <*> v .:?  "aging"
        <*> v .:?  "multiplicity"
        <*> v .:?  "comment"

instance Show ProtoTrait  where
   show p 
       | ability p /= Nothing = 
           "Ability: " ++ fromJust ( ability p )  ++ 
           " [" ++ show ( spec p ) ++ "]" ++ showXP p
           ++ " [" ++ si (bonusScore p) ++ "; " ++ maybeShow (multiplyXP p) ++ "]"
       | characteristic p /= Nothing =
           "Characteristic: " ++ fromJust ( characteristic p )  ++
           " " ++ show ( fromMaybe 0 (score p) ) ++ showAging p 
       | art p /= Nothing = 
           "Art: " ++ fromJust ( art p ) ++ showXP p
           ++ " [" ++ si (bonusScore p) ++ "; " ++ maybeShow (multiplyXP p) ++ "]"
       | spell p /= Nothing =
              "Spell: " ++ fromJust (spell p) ++ showXP p
                    ++ maybeShow (mastery p)
       | ptrait p /= Nothing = 
              "Personality Trait: " ++ fromJust (ptrait p)
                     ++ " " ++ maybeShow (score p)
       | reputation p /= Nothing = 
              "Reputation: " ++ fromJust (reputation p) ++
              " [" ++ maybeShow (locale p) ++ "]" ++ showXP p
       | virtue p /= Nothing = 
              "Virtue: " ++ fromJust (virtue p) ++ " ("
              ++ show ( fromMaybe 0 (cost p) ) ++ ")"
       | flaw p /= Nothing = 
              "Flaw: " ++ fromJust (flaw p) ++ " ("
              ++ show ( fromMaybe 0 (cost p) ) ++ ")"
       | confidence p /= Nothing = 
              fromMaybe "Confidence" (confidence p) ++ ": " ++ show (fromMaybe 0 (score p)) ++ " (" ++
              show ( fromMaybe 0 (points p) ) ++ ")"
       | other p /= Nothing = 
               fromJust (other p) ++ " " ++ show ( fromMaybe 0 ( points p ) )
       | otherwise  = error "No Trait for this ProtoTrait" 
     where si = show . fromMaybe 0

-- | 
-- = Trait

data TraitKey = AbilityKey String
           | CharacteristicKey String
           | ArtKey String
           | SpellKey String Int String 
           | PTraitKey String
           | ReputationKey String String
           | VFKey String String
           | ConfidenceKey String
           | OtherTraitKey String
           deriving (Show, Ord, Eq,Generic )
spellKeyName :: TraitKey -> String
spellKeyName ( SpellKey _ _ n ) = n
spellKeyName _ = "Error!"

instance ToJSON TraitKey
instance FromJSON TraitKey
data Ability = Ability { abilityName :: String
                       , speciality :: Maybe String
                       , abilityXP :: XPType 
                       , abilityScore :: Int 
                       , abilityBonus :: Int 
                       , abilityMultiplier :: Float
                       , abilityExcessXP :: XPType 
                       }
           deriving (Ord, Eq, Generic)
data Characteristic = Characteristic { characteristicName :: String
                                     , charScore :: Int
                                     , agingPoints :: Int }
           deriving (Ord, Eq, Generic)
data Art = Art { artName :: String
               , artXP :: XPType 
               , artScore :: Int 
               , artBonus :: Int 
               , artMultiplier :: Float
               , artExcessXP :: XPType 
               }
           deriving (Ord, Eq, Generic)
data Spell = Spell { spellName :: String
                   , spellTeFo :: String
                   , spellLevel :: Int
                   , spellXP :: XPType
                   , masteryScore :: Int
                   , spellExcessXP :: XPType
                   , spellMultiplier :: Float
                   , masteryOptions :: [String] 
                   , spellCastingScore :: Maybe Int
                   , spellTComment :: String
                   }
           deriving (Ord, Eq, Generic)

-- | Return a string of Technique/Form/Level classifying the Spell.
spellTeFoLe :: Spell -> String
spellTeFoLe sp = spellTeFo sp ++ show (spellLevel sp)

-- | Return a string of Form/Technique for sorting
spellFoTe :: Spell -> String
spellFoTe sp = drop 2 tf ++ take 2 tf
    where tf = spellTeFo sp 

-- | Personality trait
data PTrait = PTrait { ptraitName :: String, pscore :: Int }
           deriving (Ord, Eq, Generic)

-- | Reputation object 
data Reputation = Reputation { reputationName :: String  -- ^ contents of the reputation
                             , repLocale :: String       -- ^ domain or location of the reputation
                             ,  repXP :: XPType          -- ^ total XP in the reputation (used?)
                             ,  repScore :: Int          -- ^ reputation Score
                             ,  repExcessXP :: XPType    -- ^ XP towards next level in the reputation
                             }
           deriving (Ord, Eq, Generic)
data VF = VF { vfname :: String    -- ^ name of the virtue/flaw
             , vfDetail :: String  -- ^ detail, where the virtue/flaw has options
             , vfcost :: Int       -- ^ cost, should be zero for free/inferred virtues/flaws
             , vfAppliesTo :: Maybe TraitKey  -- ^ not used
             , vfMultiplicity :: Int          -- ^ number of times the virtue/flaw is take
             , vfComment :: String              -- ^ freeform comment
             }
           deriving (Ord, Eq, Generic)
data Confidence = Confidence { cname :: String, cscore :: Int, cpoints :: Int }
           deriving ( Ord, Eq, Generic)
data OtherTrait = OtherTrait { trait :: String
                             , otherScore :: Int
                             , pts :: Int 
                             , otherExcess :: Int
                             }
           deriving (Show, Ord, Eq, Generic)

data Trait = AbilityTrait Ability
           | CharacteristicTrait Characteristic
           | ArtTrait Art
           | SpellTrait Spell
           | PTraitTrait PTrait
           | ReputationTrait Reputation
           | VFTrait VF
           | ConfidenceTrait Confidence
           | OtherTraitTrait OtherTrait
           deriving (Show, Ord, Eq, Generic)
instance FromJSON Ability
instance FromJSON Characteristic 
instance FromJSON Art 
instance FromJSON Spell 
instance FromJSON PTrait 
instance FromJSON Reputation 
instance FromJSON VF 
instance FromJSON Confidence 
instance FromJSON OtherTrait 
instance FromJSON Trait  
instance ToJSON Ability
instance ToJSON Characteristic 
instance ToJSON Art 
instance ToJSON Spell 
instance ToJSON PTrait 
instance ToJSON Reputation 
instance ToJSON VF 
instance ToJSON Confidence 
instance ToJSON OtherTrait 
instance ToJSON Trait 

-- |
-- == Show instances

instance Show VF  where
   show a = vfname a ++ f sp 
          ++ " (" ++ show (vfcost a) ++ ")"
      where sp = vfDetail a
            f "" = ""
            f x = " [" ++ x ++ "]"
instance Show Confidence  where
   show a = cname a ++ ": " ++ show (cscore a) ++ " (" ++ show (cpoints a) ++ ")"
instance Show PTrait  where
   show a = ptraitName a ++ " " ++ show (pscore a)
instance Show Ability  where
   show a = abilityName a ++ " [" ++ showspec sp ++ "] "
          ++ show (abilityScore a) 
          ++ showBonus (abilityBonus a)
          ++ " (" ++ show (abilityExcessXP a) ++ "xp)"
          ++ f (abilityMultiplier a)
      where showspec Nothing = "  --  "
            showspec (Just s) = s
            sp = speciality a
            f 1 = ""
            f x = " [xp x" ++ show x ++  "]"
instance Show Characteristic  where
   show a = characteristicName a ++ " " ++ showSigned (charScore a)
          ++ showA (agingPoints a)
       where showA x | x == 0 = ""
                    | otherwise = " (" ++ show x ++ " aging points)"
instance Show Spell  where
   show a = "*" ++ spellName a ++ "* " 
            ++ spellTeFo a ++ show (spellLevel a) ++ f (spellCastingScore a)
      where f Nothing = ""
            f (Just x) = " (" ++ show x ++ ")"
instance Show Art  where
   show a = artName a ++ " " 
          ++ show (artScore a) 
          ++ showBonus (artBonus a)
          ++ " (" ++ show (artExcessXP a) ++ "xp) "
          ++ f (artMultiplier a)
      where f 1 = ""
            f x = " [xp x" ++ show x ++  "]"
instance Show Reputation  where
   show a = reputationName a ++ " [" ++ (repLocale a) ++ "] "
          ++ show (repScore a) ++ " (" ++ show (repExcessXP a) ++ ") "

showAging :: ProtoTrait -> String
showAging p | Nothing == aging p = ""
            | otherwise = " (" ++ show  pt ++ " aging points)"
    where pt = maybeInt $ aging p
showXP :: ProtoTrait -> String
showXP p = " " ++ show ( fromMaybe 0 (xp p) ) ++ "xp"



-- |
-- = Computing Traits

computeOther :: ProtoTrait -> OtherTrait
computeOther p
    | spell p == Nothing = error "Not an properly formatted trait"
    | otherwise =
           OtherTrait { trait = fromJust (other p) 
                      , pts = fromMaybe 0 ( points p ) 
                      , otherScore = s
                      , otherExcess = y
                      }
                 where (s,y) = getAbilityScore (points p)



-- |
-- = Filtering and Advancement - the TraitType class

class TraitType t where
    -- | Extract traits of the given type from a generic list of Trait objects.
    -- It returns a pair of lists with the selected traits in the first list
    -- and the remaining traits in the other.
    filterTrait :: [ Trait ] -> ( [ t ], [ Trait ] )
    filterTrait ts = y where (_,y) = filterTrait' (ts,([],[]))

    -- | Recursive helper for `filterTrait`
    filterTrait' :: ( [ Trait ], ( [ t ], [ Trait ] ) )
                  -> ( [ Trait ], ( [ t ], [ Trait ] ) )
    filterTrait' ([],y) = ([],y)
    filterTrait' (x:xs,(ys,zs)) | isNothing ab  = filterTrait' (xs,(ys,x:zs))
                                | otherwise = filterTrait' (xs,(fromJust ab:ys,zs))
        where ab = getTrait x

    -- | Return the specific trait from the generic Trait, or Nothing if the type does not match.
    getTrait :: Trait -> Maybe t

    -- | Convert a ProtoTrait (advancement) to a new trait object.
    computeTrait :: ProtoTrait -> Maybe t

instance TraitType Characteristic where
    getTrait (CharacteristicTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p
       | characteristic p == Nothing = Nothing
       | otherwise = Just $
          Characteristic { characteristicName = fromJust ( characteristic p ) 
                , charScore = fromMaybe 0 (score p) + fromMaybe 0 (bonusScore p)
                , agingPoints = maybeInt (aging p) }
instance TraitType VF where
    getTrait (VFTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p 
       | virtue p /= Nothing = Just $ vf1 { vfname = fromJust (virtue p) }
       | flaw p /= Nothing = Just $ vf1 { vfname = fromJust (flaw p) }
       | otherwise = Nothing
      where vf1 = VF { vfname = "", vfcost = maybeInt (cost p), vfDetail = fromMaybe "" $ detail p
                    , vfAppliesTo = Nothing
                    , vfMultiplicity = fromMaybe 1 $ multiplicity p
                    , vfComment = fromMaybe "" $ comment p }
instance TraitType Ability where
    getTrait (AbilityTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p
       | ability p == Nothing = Nothing
       | otherwise = Just $
           Ability { abilityName = fromJust ( ability p ) 
                , speciality = spec p
                , abilityXP = maybeInt (xp p)
                , abilityScore = s
                , abilityExcessXP = y
                , abilityBonus = fromMaybe 0 $ bonusScore p
                , abilityMultiplier = fromMaybe 1.0 $ multiplyXP p
                }
     where (s,y) = getAbilityScore (xp p)
instance TraitType Art where
    getTrait (ArtTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p
        | art p == Nothing = Nothing
        | otherwise = Just $
            Art { artName = fromJust ( art p ) 
                , artXP = x
                , artScore = s
                , artExcessXP = y
                , artBonus = fromMaybe 0 $ bonusScore p
                , artMultiplier = fromMaybe 1.0 $ multiplyXP p
                }
     where y = x - pyramidScore s
           s = scoreFromXP x
           x = fromMaybe 0 (xp p) 
instance TraitType Spell where
    getTrait (SpellTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p | spell p == Nothing = Nothing
                   | otherwise =  Just sp 
          where sp = Spell { spellName = fromJust (spell p)
                      , spellLevel = fromMaybe 0 $ level p
                      , spellTeFo = fromMaybe "TeFo" $ tefo p
                      , spellXP = fromMaybe 0 (xp p)
                      , masteryScore = s
                      , masteryOptions = maybeList (mastery p)
                      , spellExcessXP = y
                      , spellMultiplier = fromMaybe 1.0 $ multiplyXP p
                      , spellCastingScore = Nothing
                      , spellTComment = fromMaybe "" $ comment p
                      }
                (s,y) = getAbilityScore (xp p)
instance TraitType Reputation where
    getTrait (ReputationTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p
       | reputation p == Nothing = Nothing
       | otherwise = Just $
           Reputation { reputationName = fromJust (reputation p)
                      , repLocale = fromMaybe "" (locale p)
                      , repXP = fromMaybe 0 (xp p)
                      , repScore = s
                      , repExcessXP = y
                      }
     where (s,y) = getAbilityScore (xp p)
instance TraitType PTrait where
    getTrait (PTraitTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p 
       | ptrait p /= Nothing = Just $ PTrait 
                           { ptraitName = fromJust ( ptrait p )
                           , pscore = fromMaybe 0 (score p) }
       | otherwise = Nothing
instance TraitType Confidence where
    getTrait (ConfidenceTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p 
       | ptrait p /= Nothing = Just $ Confidence 
                           { cname = fromMaybe "Confidence" ( confidence p )
                           , cscore = fromMaybe 0 (score p) 
                           , cpoints = fromMaybe 0 (points p) 
                           }
       | otherwise = Nothing


-- |
-- = Sorting and Advancement 

(<:) :: (TraitLike t1, TraitLike t2) => t1 -> t2 -> Bool
(<:) p1 p2 = traitKey p1 < traitKey p2

{-
(>:) :: (TraitLike t1, TraitLike t2) => t1 -> t2 -> Bool
(>:) p1 p2 = p2 <: p1
-}


sortTraits :: TraitLike t => [ t ] -> [ t ]
sortTraits = sortBy f
       where f x y | x <: y = LT
                   | y <: x = GT
                   | otherwise = EQ

-- |
-- == the TraitLike class

class TraitLike t where
    traitKey :: t -> TraitKey
    advanceTrait :: ProtoTrait -> t -> t
    advanceTrait _ x = x
    toTrait :: t -> Trait

instance TraitLike Trait where
    traitKey (CharacteristicTrait x) = traitKey x
    traitKey (AbilityTrait x) = traitKey x
    traitKey (ArtTrait x) = traitKey x
    traitKey (SpellTrait x) = traitKey x
    traitKey (ReputationTrait x) = traitKey x
    traitKey (VFTrait x) = traitKey x
    traitKey (PTraitTrait x) = traitKey x
    traitKey (OtherTraitTrait x) = traitKey x
    traitKey (ConfidenceTrait x) = traitKey x
    advanceTrait a (CharacteristicTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (AbilityTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (ArtTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (SpellTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (ReputationTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (VFTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (PTraitTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (OtherTraitTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (ConfidenceTrait x) = toTrait $ advanceTrait a x
    toTrait = id

updateBonus :: Maybe Int -> Ability -> Ability
updateBonus Nothing a = a 
updateBonus (Just x) a = a { abilityBonus = x + abilityBonus a }

updateArtBonus :: Maybe Int -> Art -> Art
updateArtBonus Nothing a = a 
updateArtBonus (Just x) a = a { artBonus = x + artBonus a }

instance TraitLike PTrait where
    traitKey x = PTraitKey $ ptraitName x
    toTrait = PTraitTrait
    advanceTrait _ x = trace "Warning! Advancement not implemented for personality traits"  x
instance TraitLike VF where
    traitKey x = VFKey (vfname x) (vfDetail x)
    toTrait = VFTrait
instance TraitLike Ability where
    traitKey x = AbilityKey $ abilityName x
    toTrait = AbilityTrait
    advanceTrait a x = 
          updateBonus (bonusScore a) $ um (multiplyXP a) $
          updateAbilitySpec (spec a) $ updateAbilityXP y x
      where y = calcXP m (abilityExcessXP x) (xp a) 
            m = abilityMultiplier x
            um Nothing ab = ab 
            um abm ab = ab { abilityMultiplier = fromMaybe 1.0 abm }
instance TraitLike Art where
    traitKey x = ArtKey $ take 2 $ artName x
    toTrait = ArtTrait
    advanceTrait a x = 
          updateArtBonus (bonusScore a) $ um (multiplyXP a) $ 
          updateArtXP y x 
      where y = calcXP m (artExcessXP x) (xp a) 
            m = artMultiplier x
            um Nothing ab = ab 
            um abm ar = ar { artMultiplier = fromMaybe 1.0 abm }
instance TraitLike Spell where
    traitKey x = SpellKey (spellFoTe x) (spellLevel x) (spellName x ) 
    toTrait = SpellTrait
    advanceTrait a x = updateSpellXP y $ updateSpellMastery ms x
      where y = (spellExcessXP x) + (fromMaybe 0 $ xp a)
            ms = maybeList $ mastery a
instance TraitLike Reputation where
    traitKey x = ReputationKey ( reputationName x ) ( repLocale x )
    toTrait = ReputationTrait
    advanceTrait a x = updateRepXP y x
      where y = (repExcessXP x) + (fromMaybe 0 $ xp a)
instance TraitLike Characteristic where
    traitKey x = CharacteristicKey ( characteristicName x ) 
    toTrait = CharacteristicTrait
    advanceTrait _ x = trace "Warning! Advancement not implemented for characteristics"  x
instance TraitLike Confidence where
    traitKey p = ConfidenceKey $ cname p
    toTrait = ConfidenceTrait
    advanceTrait a = updateCScore (score a) . updateCPoints (points a) 
       where updateCScore Nothing x = x
             updateCScore (Just y) x = x { cscore = y }
             updateCPoints Nothing x = x
             updateCPoints (Just y) x = x { cpoints = y + cpoints x }
instance TraitLike  OtherTrait where
    traitKey x = OtherTraitKey ( trait x ) 
    toTrait = OtherTraitTrait
    advanceTrait _ x = trace "Warning! Advancement not implemented for OtherTrait"  x


instance TraitLike ProtoTrait where
   traitKey p
       | ability p /= Nothing = AbilityKey $ fromJust $ ability p 
       | characteristic p /= Nothing = CharacteristicKey $ fromJust $ characteristic p 
       | art p /= Nothing = ArtKey $ take 2 $ fromJust $ art p 
       | spell p /= Nothing = SpellKey (fromMaybe "" $ tefo p)
                           (fromMaybe 0 $ level p ) ( fromJust $ spell p ) 
       | ptrait p /= Nothing = PTraitKey $ fromJust $ ptrait p
       | reputation p /= Nothing = ReputationKey (fromJust (reputation p)) (fromMaybe "" (locale p))
       | virtue p /= Nothing = VFKey ( fromJust (virtue p) ) (fromMaybe "" $ detail p)
       | flaw p /= Nothing = VFKey ( fromJust (flaw p) ) (fromMaybe "" $ detail p)
       | confidence p /= Nothing = ConfidenceKey $ fromMaybe "Confidence" $ confidence p
       | other p /= Nothing = OtherTraitKey $ fromJust $ other p
       | otherwise  = error "No Trait for this ProtoTrait" 

   toTrait p 
      | ability p /= Nothing = AbilityTrait $ fromJust $ computeTrait p
      | characteristic p /= Nothing = CharacteristicTrait $  fromJust $ computeTrait p
      | art p /= Nothing = ArtTrait $ fromJust $ computeTrait p
      | spell p /= Nothing = SpellTrait $ fromJust $ computeTrait p
      | ptrait p /= Nothing = PTraitTrait $
           PTrait { ptraitName = fromJust (ptrait p)
                  , pscore = fromMaybe 0 (score p)
                  } 
      | reputation p /= Nothing = ReputationTrait $ fromJust $ computeTrait p
      | virtue p /= Nothing = VFTrait $ fromJust $ computeTrait p
      | flaw p /= Nothing = VFTrait $ fromJust $ computeTrait p
      | confidence p /= Nothing = ConfidenceTrait $ 
           Confidence { cname = fromMaybe "Confidence" (confidence p)
                      , cscore = fromMaybe 0 (score p)
                      , cpoints = fromMaybe 0 (points p) }
      | other p /= Nothing = OtherTraitTrait $ computeOther p
      | otherwise  = error "No Trait for this ProtoTrait" 

-- |
-- == Auxiliary update functions

updateAbilitySpec :: Maybe String -> Ability -> Ability
updateAbilitySpec Nothing a = a
updateAbilitySpec (Just x) a = a { speciality = Just x }

updateAbilityXP :: XPType -> Ability -> Ability
updateAbilityXP x ab | x < tr = ab { abilityExcessXP = x }
                     | otherwise = updateAbilityXP (x-tr) $ ab { abilityScore = sc+1 }
    where sc = abilityScore ab
          tr = fromIntegral (sc+1)*5

updateRepXP :: XPType -> Reputation -> Reputation
updateRepXP x ab | x < tr = ab { repExcessXP = x }
                 | otherwise = updateRepXP (x-tr) $ ab { repScore = sc+1 }
    where sc = repScore ab
          tr = fromIntegral $ (sc+1)*5

updateArtXP :: XPType -> Art -> Art
updateArtXP x ab | x < tr = ab { artExcessXP = x }
                 | otherwise = updateArtXP (x-tr) $ ab { artScore = sc+1 }
    where sc = artScore ab
          tr = fromIntegral (sc+1)

updateSpellXP :: XPType -> Spell -> Spell
updateSpellXP x ab | x < tr = ab { spellExcessXP = x }
                   | otherwise = updateSpellXP (x-tr) $ ab { masteryScore = sc+1 }
    where sc = masteryScore ab
          tr = fromIntegral $ (sc+1)*5
updateSpellMastery :: [String] -> Spell -> Spell
updateSpellMastery ms t = t { masteryOptions = (masteryOptions t) ++ ms }

-- |
-- = Advancement

-- | Apply a list of ProtoTrait advancements to a list of Traits.
--
-- This is the main function used by other modules when characters are
-- advanced.
advance :: [ ProtoTrait ] -> [ Trait ] -> [ Trait ]
advance [] ys = ys
advance (x:xs) [] = advance xs [toTrait x]
advance (x:xs) (y:ys) 
    | x <: y = advance xs (toTrait x:y:ys)
    | y <: x = y:advance (x:xs) ys
    | otherwise = advance xs (adv x y:ys)
    where adv a b = toTrait $ advanceTrait a b

