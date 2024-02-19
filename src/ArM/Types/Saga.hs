{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types and basic functions to handle sagas, that is the
-- top level object of the data model
--
-----------------------------------------------------------------------------
module ArM.Types.Saga ( Saga(..)
                      , defaultSaga
                      , sagaFromRDF
                      ) where

import ArM.Internal.Aux

import           Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import           Swish.VarBinding  (vbMap)

import           Data.Aeson
import           ArM.Types.RDF
import           ArM.Rules.Aux
import           ArM.Resources
import           ArM.KeyPair

data Saga = Saga { sagaID :: RDFLabel
                 , sagaTitle :: String
                 , schemaFiles :: [String]
                 , resourceFiles :: [String]
                 , characterFiles :: [String]
                 , sagaGraph :: RDFGraph
                 }

defaultSaga :: Saga 
defaultSaga = Saga { sagaID = armRes "noSuchSaga"
                 , sagaTitle = "No Title"
                 , schemaFiles = []
                 , resourceFiles = []
                 , characterFiles = []
                 , sagaGraph = emptyGraph
                 }


instance FromRDFGraph Saga where 
   fromRDFGraph g label = fixSaga $ defaultSaga
                 { sagaID = label
                 , sagaGraph = g
                 }

-- | Return a saga object from an RDFGraph.
-- The graph should contain one and only one saga.
sagaFromRDF :: RDFGraph -> Saga
sagaFromRDF g = fromRDFGraph g sid
    where sid = head $ sagaFromGraph g


-- | Complete a saga object by extracting file names from the graph.
fixSaga :: Saga -> Saga
fixSaga s = s { schemaFiles = getSchemaFiles sid g
              , resourceFiles = getResourceFiles sid g 
              , characterFiles = getCharacterFiles sid g 
              }
          where g = sagaGraph s
                sid = sagaID s

-- | Get the labels of all sagas in a given graph.
sagaFromGraph :: RDFGraph -> [RDFLabel]
sagaFromGraph = uniqueSort . f . map (`vbMap` cVar) 
                . Q.rdfQueryFind ( listToRDFGraph  [ arc cVar typeRes (armRes "Saga") ] )
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs

getFiles :: String -> RDFLabel -> RDFGraph -> [String]
getFiles ft s = f . map rdfToString . f 
                   . map (`vbMap` (Var "file")) . parsegraph 
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs
          parsegraph = Q.rdfQueryFind $ listToRDFGraph  [ a ]
          a = arc s (armRes ft) (Var "file") 
getResourceFiles :: RDFLabel -> RDFGraph -> [String]
getResourceFiles = getFiles "hasResourceFile"
getSchemaFiles :: RDFLabel -> RDFGraph -> [String]
getSchemaFiles = getFiles "hasSchemaFile"
getCharacterFiles :: RDFLabel -> RDFGraph -> [String]
getCharacterFiles = getFiles "hasCharacterFile"


instance ToJSON Saga where 
    toJSON c = toJSON $ p x xs
        where x = KeyValuePair (armRes "sagaID") $ sagaID c
              xs = fromRDFGraph ( sagaGraph c ) ( sagaID c )
              p y (KeyPairList ys) = KeyPairList (y:ys) 
