{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.SagaFile
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types and basic functions to handle sagas, that is the
-- top level object of the data model
--
-----------------------------------------------------------------------------
module ArM.Types.SagaFile ( SagaFile(..)
                          , defaultSagaFile
                          , sagaFromRDF
                          ) where

import ArM.Internal.Aux
import ArM.Debug.Trace

import           Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import           Swish.VarBinding  (vbMap)

import           Data.Aeson
import           ArM.Types.RDF
import           ArM.Rules.Aux
import           ArM.Swish.Resources
import           ArM.KeyPair
import Data.Maybe(catMaybes)

data SagaFile = SagaFile { sagaID :: RDFLabel
                 , sagaTitle :: String
                 , schemaFiles :: [String]
                 , resourceFiles :: [String]
                 , characterFiles :: [String]
                 , sagaGraph :: RDFGraph
                 }

defaultSagaFile :: SagaFile 
defaultSagaFile = SagaFile { sagaID = armRes "noSuchSaga"
                 , sagaTitle = "No Title"
                 , schemaFiles = []
                 , resourceFiles = []
                 , characterFiles = []
                 , sagaGraph = emptyGraph
                 }


instance FromRDFGraph SagaFile where 
   fromRDFGraph g label = fixSaga $ defaultSagaFile
                 { sagaID = label
                 , sagaGraph = g
                 , sagaTitle = ttrace $ getTitle label g
                 }

-- | Return a saga object from an RDFGraph.
-- The graph should contain one and only one saga.
sagaFromRDF :: RDFGraph -> SagaFile
sagaFromRDF g = fromRDFGraph g sid
    where sid = h $ sagaFromGraph g
          h []  = error "No saga"
          h xs = head xs


-- | Complete a saga object by extracting file names from the graph.
fixSaga :: SagaFile -> SagaFile
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

getTitle :: RDFLabel -> RDFGraph -> String
getTitle s = f . map rdfToString . catMaybes 
                   . map (`vbMap` (Var "title")) . parsegraph 
    where f [] = "No title"
          f (Nothing:xs) = f xs
          f (Just x:_) = x
          parsegraph = Q.rdfQueryFind $ listToRDFGraph  [ a ]
          a = arc s (armRes "hasTitle") (Var "title") 

getFiles :: String -> RDFLabel -> RDFGraph -> [String]
getFiles ft s = catMaybes . map rdfToString . catMaybes 
                   . map (`vbMap` (Var "file")) . parsegraph 
    where parsegraph = Q.rdfQueryFind $ listToRDFGraph  [ a ]
          a = arc s (armRes ft) (Var "file") 
getResourceFiles :: RDFLabel -> RDFGraph -> [String]
getResourceFiles = getFiles "hasResourceFile"
getSchemaFiles :: RDFLabel -> RDFGraph -> [String]
getSchemaFiles = getFiles "hasSchemaFile"
getCharacterFiles :: RDFLabel -> RDFGraph -> [String]
getCharacterFiles = getFiles "hasCharacterFile"


instance ToJSON SagaFile where 
    toJSON c = toJSON $ p x xs
        where x = KeyValuePair (armRes "sagaID") $ sagaID c
              xs = fromRDFGraph ( sagaGraph c ) ( sagaID c )
              p y (KeyPairList ys) = KeyPairList (y:ys) 
