{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Resources
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- This module defines namespaces, URIs, and filenames used in the project.
-- It also defines utility functions to create RDFLabel-s from strings.
--
-- Much of this data should be made configurable.
--
-----------------------------------------------------------------------------
module ArM.Resources where

import Swish.Namespace
import Swish.RDF.Graph
import Network.URI
import qualified Data.Text as T
import qualified Swish.QName as QN
import           Data.Text (unpack)

-- * Ontology Files

armFile = "Ontology/arm.ttl"
resourceFile = "Ontology/resources.ttl"
characterFile = "Test/cieran.ttl"
baseURI = Nothing
    
-- * URIs and NameSpaces

auth = URIAuth "" "hg.schaathun.net" ""
armURI = URI { uriScheme = "https:",
           uriAuthority = Just auth,
           uriPath = "/armchar/schema",
           uriQuery = "",
           uriFragment = "#" }
armrURI = URI { uriScheme = "https:",
           uriAuthority = Just auth,
           uriPath = "/armchar/resources",
           uriQuery = "",
           uriFragment = "#" }
rulesURI = URI { uriScheme = "https:",
           uriAuthority = Just auth,
           uriPath = "/armchar/rules",
           uriQuery = "",
           uriFragment = "#" }
armcharURI = URI { uriScheme = "https:",
           uriAuthority = Just auth,
           uriPath = "/armchar/character/",
           uriQuery = "",
           uriFragment = "" }
armNS = makeNamespace (Just $ T.pack "arm") armURI
rulesNS = makeNamespace (Just $ T.pack "armrules") armURI

-- * Utility functions to manage local names

-- | Define a local name from a String
newLName s = case (QN.newLName $ T.pack s) of
   (Nothing) -> QN.emptyLName
   (Just ln) -> ln

-- | Extract the local name (as String) from an RDFLabel.
getLocalID :: RDFLabel -> Maybe String
getLocalID lab = f $ fromRDFLabel lab 
      where f Nothing = Nothing
            f (Just x) = Just $ unpack $ QN.getLName $ getScopeLocal x

-- * Convenience functions to make ontology labels

makeSN s = makeScopedName (Just $ T.pack "arm") armURI (newLName s)
armRes :: String -> RDFLabel
armRes = Res . makeSN
armcharRes :: String -> RDFLabel
armcharRes s = Res $ makeScopedName (Just $ T.pack "armchar") armcharURI (newLName s)
armrRes :: String -> RDFLabel
armrRes s = Res $ makeScopedName (Just $ T.pack "armr") armrURI (newLName s)

-- * Ontology Labels

isCharacterLabel = armRes  "isCharacter"
repeatableLabel = armRes  "RepeatableTrait"
xptraitLabel = armRes  "XPTrait"
accelleratedtraitLabel = armRes  "AccelleratedTrait"
addXPLabel = armRes  "addedXP"
totalXPLabel = armRes "hasTotalXP" 
hasXPLabel = armRes "hasXP" 

-- | An RDFLabel used as a kind of Null pointer for traits and items.
noSuchTrait = armRes "noSuchTrait" 
-- | An RDFLabel used as a kind of Null pointer for advancements.
noSuchAdvancement = armRes "noSuchAdvancement" 
-- | An RDFLabel used as a kind of Null pointer for characters.
noSuchCharacter = armRes "noSuchCharacter" 
atSeason = armRes "atSeason" 
inYear = armRes "inYear" 
advancementType = armRes "Advancement" 
hasAdvancementIndex = armRes "hasAdvancementIndex" 
hasAdvancementType = armRes "hasAdvancementType" 
hasAdvancementTypeString = armRes "hasAdvancementTypeString" 
prefixedidRes = armRes "prefixedid" 
armViewProperty = armRes "ViewProperty" 
armPersistentProperty = armRes "PersistentProperty" 
armCharacterProperty = armRes "CharacterProperty" 
armCharacter = armRes "Character" 
