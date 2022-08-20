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
module ArM.Types.Saga where

import ArM.Internal.Aux

import           Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import qualified Swish.RDF.VarBinding as VB 
import           Swish.VarBinding  (vbMap)
import           Data.Maybe (fromJust)
import           Data.List (sort)
import qualified ArM.KeyPair as KP
import           ArM.Types.Character 
import           ArM.Rules.Aux
import           ArM.Resources

data Saga = Saga { sagaID :: RDFLabel
                 , sagaTitle :: String
                 , schemaFile :: String
                 , resourceFiles :: [String]
                 , characterFiles :: [String]
                 }

defaultSaga = Saga { sagaID = armRes "noSuchSaga"
                 , sagaTitle = "No Title"
                 , schemaFile = "/dev/null"
                 , resourceFiles = []
                 , characterFiles = []
                 }


-- | Get the labels of all sagas in a given graph.
sagaFromGraph :: G.RDFGraph -> [G.RDFLabel]
sagaFromGraph = uniqueSort . f . map (`vbMap` cVar) . parsegraph
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs
          parsegraph = Q.rdfQueryFind 
                     $ listToRDFGraph  [ arc cVar typeRes (armRes "Saga") ]

-- | Construct a query to get all
-- arm:CharacterProperty triples for a given subject.
query c = listToRDFGraph 
   [ arc c (G.Var "property") (G.Var "value") ]

-- |
-- = Instances - Load Character object from graph

instance FromRDFGraph Saga where
   fromRDFGraph g label = defaultSaga {
                 sagaID = label
                 }
