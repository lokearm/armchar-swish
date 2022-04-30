{-# LANGUAGE OverloadedStrings #-}
module ArM.Resources where

import Swish.Namespace
import Swish.RDF.Graph
import Network.URI
import qualified Data.Text as T
import qualified Swish.QName as QN

armFile = "Ontology/arm.ttl"
resourceFile = "Ontology/resources.ttl"
characterFile = "Test/cieran.ttl"
baseURI = Nothing

prefixes = "@prefix owl: <http://www.w3.org/2002/07/owl#> . "
   ++ "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . "
   ++ "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> . "
   ++ "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> . "
   ++ "@prefix foaf: <http://xmlns.com/foaf/0.1/>. "
   ++ "@prefix dc: <http://purl.org/dc/elements/1.1/> . "
   ++ "@prefix arm: <https://hg.schaathun.net/armchar/schema#> . "
   ++ "@prefix armr: <https://hg.schaathun.net/armchar/resources#> . "
   ++ "@prefix armchar: <https://hg.schaathun.net/armchar/character/> . "

    
-- URIs
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

-- | Define a local name from a String
newLName s = case (QN.newLName $ T.pack s) of
   (Nothing) -> QN.emptyLName
   (Just ln) -> ln

makeSN s = makeScopedName (Just $ T.pack "arm") armURI (newLName s)


armcharRes :: String -> RDFLabel
armcharRes s = Res $ makeScopedName (Just $ T.pack "armchar") armcharURI (newLName s)
armrRes :: String -> RDFLabel
armrRes s = Res $ makeScopedName (Just $ T.pack "armr") armrURI (newLName s)

isCharacterLabel = Res $ makeSN  "isCharacter"
repeatableLabel = Res $ makeSN  "RepeatableTrait"
xptraitLabel = Res $ makeSN  "XPTrait"
accelleratedtraitLabel = Res $ makeSN  "AccelleratedTrait"
addXPLabel = Res $ makeSN  "addedXP"
totalXPLabel = Res $ makeSN "hasTotalXP" 
scoreLabel = Res $ makeSN "hasScore" 
hasXPLabel = Res $ makeSN "hasXP" 
springLabel = Res $ makeSN "Spring" 
summerLabel = Res $ makeSN "Summer" 
autumnLabel = Res $ makeSN "Autumn" 
winterLabel = Res $ makeSN "Winter" 
noSuchTrait = Res $ makeSN "noSuchTrait" 
noSuchAdvancement = Res $ makeSN "noSuchAdvancement" 
noSuchCharacter = Res $ makeSN "noSuchCharacter" 
inSeason = Res $ makeSN "inSeason" 
atSeason = Res $ makeSN "atSeason" 
inYear = Res $ makeSN "inYear" 
advancementType = Res $ makeSN "Advancement" 
hasAdvancementIndex = Res $ makeSN "hasAdvancementIndex" 
hasAdvancementType = Res $ makeSN "hasAdvancementType" 
hasAdvancementTypeString = Res $ makeSN "hasAdvancementTypeString" 
prefixedidRes = Res $ makeSN "prefixedid" 
armViewProperty = Res $ makeSN "armViewProperty" 
armEditableProperty = Res $ makeSN "armEditableProperty" 
