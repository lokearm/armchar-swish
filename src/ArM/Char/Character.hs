{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- The Character and CharacterSheet data types and corresponding functions.
--
-- This module provides the functions to get character sheets
-- from character data.
--
-- Pre-game character design has not yet been implemented.
--
-----------------------------------------------------------------------------
module ArM.Char.Character ( Character(..)
                          , defaultCharacter
                          , KeyPairList(..)
                          , KeyPair(..)
                          , FieldValue(..)
                          , advanceCharacter
                          ) where

import GHC.Generics
import Data.List (sort)
-- import Swish.RDF.Graph (RDFLabel)
-- import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Key
-- import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM

import ArM.Debug.Trace
import ArM.Char.Trait
-- import ArM.Types.Season

type CharTime = Maybe String

-- = KeyPairList

data FieldValue = TextValue T.Text
                | IntValue Int
                | ObjectValue Value
       deriving (Eq)
data KeyPair = KeyPair { key :: String, value :: FieldValue }
       deriving (Eq)
data KeyPairList = KeyPairList [ KeyPair ]
       deriving (Eq)

instance FromJSON KeyPairList  where
  parseJSON = withObject "KeyPairList" $ \obj ->
    trace ( "parseJSON for KeyPairList " ++ show obj) $
    return $ KeyPairList 
           $ map ( \ (k,y) -> KeyPair (toString k) (pValue y) )
           $ KM.toList obj
instance ToJSON KeyPairList where 
    toJSON (KeyPairList t) = object $ map pairToJSON t

pValue :: Value -> FieldValue
pValue (Number x) = IntValue $ round x
pValue (String x) = TextValue x
pValue (x) = ObjectValue x

pairToJSON :: KeyPair -> (Key,Value)
pairToJSON (KeyPair a (IntValue b)) = ((fromString a), (toJSON b))
pairToJSON (KeyPair a (TextValue b)) = ((fromString a), (toJSON b))
pairToJSON (KeyPair a (ObjectValue b)) = ((fromString a), (b))

-- = Character

data Character = Character {
         charID :: String
         , charGlance :: KeyPairList
         , charData :: KeyPairList
         , pregameAdvancement :: [ Advancement ]
         , charAdvancement :: [ Advancement ]
       }  deriving (Eq,Generic)



defaultCharacter :: Character 
defaultCharacter = Character { charID = "N/A"
                             , charGlance = KeyPairList []
                             , charData = KeyPairList []
                             , pregameAdvancement = [ ]
                             , charAdvancement = [ ]
       }  


instance ToJSON Character where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Character where
    parseJSON = withObject "Character" $ \v -> Character
        <$> v .: "charID"
        <*> v .: "charGlance"
        <*> v .: "charData"
        <*> fmap listNothing ( v .:? "pregameAdvancement" )
        <*> fmap listNothing ( v .:? "charAdvancement" )

listNothing :: Maybe [a] -> [a]
listNothing Nothing = []
listNothing (Just xs) = xs
-- = Advancement

data Season = Spring | Summer | Autumn | Winter 
   deriving (Show,Ord,Eq)
data AdvancementType = Practice | Exposure | Adventure 
                     | Teaching | Training | Reading | VisStudy
   deriving (Show,Ord,Eq)
data ExposureType = LabWork | Teach | Train 
                  | Writing | Copying | OtherExposure | NoExposure
   deriving (Show,Ord,Eq)

data Advancement = PreGame { stage :: String }
                 | Advancement { mode :: Maybe String
                               , season :: CharTime
                               , narrative :: Maybe String
                               , totalXP :: Maybe Int
                               , changes :: [ ProtoTrait ]
                               }
   deriving (Eq,Generic,Show)

instance ToJSON Advancement where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Advancement where
    parseJSON = withObject "Advancement" $ \v -> Advancement
        <$> v .:? "mode"
        <*> v .:? "season"
        <*> v .:? "narrative"
        <*> v .:? "totalXP"
        <*> fmap listNothing ( v .:? "changes" )

-- = Show Instances
instance Show FieldValue where
   show (IntValue x) = show x
   show (TextValue x) = show x
   show x = show x
instance Show KeyPair where
   show (KeyPair x  y) = x ++ ":\t" ++ show y ++ "\n"
instance Show KeyPairList where
   show (KeyPairList xs) = ( foldl (++) "" $ map show xs )
instance Show Character where
   show c = ( show $ charGlance c ) ++ ( show $ charData c )

-- = Advancement of Character

advanceTraits :: [ ProtoTrait ] -> [ ProtoTrait ] -> [ ProtoTrait ]
advanceTraits [] ys = ys
advanceTraits ys [] = ys
advanceTraits (x:xs) (y:ys) 
    | x <: y = x:advanceTraits xs (y:ys)
    | y <: x = y:advanceTraits (x:xs) ys
    | otherwise = advanceTrait x y:advanceTraits xs ys

advanceCharacter :: Character -> [ ( CharTime, [ ProtoTrait ] ) ]
advanceCharacter c = advanceCharacter' (charAdvancement c)
                   $ advanceCharacter' (pregameAdvancement c) []
advanceCharacter' :: [ Advancement ] -> [ ( CharTime, [ ProtoTrait ] ) ] 
                                    -> [ ( CharTime, [ ProtoTrait ] ) ]
advanceCharacter' [] cs = cs
advanceCharacter' (a:as) [] = advanceCharacter' as [ ( season a, changes a ) ] 
advanceCharacter' (a:as) ((t,xs):cs) = 
    advanceCharacter' (sort as) bs
       where bs = ( ( season a, advanceTraits (changes a) xs):(t,xs):cs ) 



