{-# LANGUAGE DeriveGeneric #-}
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
                          , KeyPairList(..)
                          , KeyPair(..)
                          ) where

import GHC.Generics
import Swish.RDF.Graph (RDFLabel)
-- import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Key
-- import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM

import ArM.Resources
import ArM.Types.RDF
import ArM.Debug.Trace

data KeyPair = KeyPair { key :: String,
                         value :: Either T.Text Int }
       deriving (Eq)
data KeyPairList = KeyPairList [ KeyPair ]
       deriving (Eq)

data Character = Character {
         charID :: String
         , charGlance :: KeyPairList
         , charData :: KeyPairList
         -- , charAdvancement :: [ Advancement ]
       }  deriving (Eq,Generic)

instance Show KeyPair where
   show (KeyPair x (Left y)) = x ++ ":\t" ++ show y ++ "\n"
   show (KeyPair x (Right y)) = x ++ ":\t" ++ show y ++ "\n"
instance Show KeyPairList where
   show (KeyPairList xs) = ( foldl (++) "" $ map show xs )
instance Show Character where
   show c = ( show $ charGlance c ) ++ ( show $ charData c )


defaultCharacter :: Character 
defaultCharacter = Character { charID = "N/A"
                             , charGlance = KeyPairList []
                             , charData = KeyPairList []
                             -- , charAdvancement = [ ]
       }  

{-
instance ToJSON Character where 
    toJSON c = toJSON $ p x xs
        where x = KeyPair (armRes "isCharacter") $ charID c
              xs = charData c 
              p y (KeyPairList ys) = KeyPairList (y:ys) 

instance FromJSON Character where 
    parseJSON val = fmap kpToChar $ parseJSON val
       where kpToChar (KeyPairList xs) = defaultCharacter {
                      charID = fromJ $ getProperty (armRes "isCharacter") xs,
                      charData = KeyPairList xs
                      }
             fromJ Nothing = noSuchCharacter
             fromJ (Just x) = x
-}

instance ToJSON Character where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Character

instance FromJSON KeyPairList  where
  parseJSON = withObject "KeyPairList" $ \obj ->
    trace ( "parseJSON for KeyPairList " ++ show obj) $
    return $ KeyPairList 
           $ map ( \ (k,y) -> KeyPair (toString k) (pValue y) )
           $ KM.toList obj
instance ToJSON KeyPairList where 
    toJSON (KeyPairList t) = object $ map pairToJSON t

pValue :: Value -> Either T.Text Int
pValue (Number x) = Right $ round x
pValue (String x) = Left x
pValue (x) = Left $ T.pack $ show x

pairToJSON :: KeyPair -> (Key,Value)
pairToJSON (KeyPair a (Right b)) = ((fromString a), (toJSON b))
pairToJSON (KeyPair a (Left b)) = ((fromString a), (toJSON b))
