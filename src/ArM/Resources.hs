module ArM.Resources where

import Swish.Namespace
import Network.URI
import qualified Data.Text as T

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
   ++ "@prefix armab: <https://hg.schaathun.net/armchar/resources/abilities#> . "
   ++ "@prefix armchar: <https://hg.schaathun.net/armchar/character/> . "

    
-- URIs
auth = URIAuth "" "hg.schaathun.net" ""
armURI = URI { uriScheme = "https:",
           uriAuthority = Just auth,
           uriPath = "/armchar/schema",
           uriQuery = "",
           uriFragment = "#" }
rulesURI = URI { uriScheme = "https:",
           uriAuthority = Just auth,
           uriPath = "/armchar/rules",
           uriQuery = "",
           uriFragment = "#" }
armNS = makeNamespace (Just $ T.pack "arm") armURI
rulesNS = makeNamespace (Just $ T.pack "armrules") armURI
