{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Query.Metadata
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Queries to build JSON files for character sheet metadata.
--
-----------------------------------------------------------------------------
module ArM.Query.Metadata ( getMetaData ) where

import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import           ArM.Resources 
import           ArM.KeyPair
import ArM.Rules.Aux
import Swish.RDF.Vocabulary.RDF


arcs :: G.RDFGraph
arcs = listToRDFGraph 
   [ G.arc idVar typeRes csRes                          -- type CharacterSheet
   , G.arc propertyVar typeRes (armRes "ViewProperty") -- property of interest
   , G.arc idVar propertyVar valueVar                   -- triple of interest
   , G.arc propertyVar labelRes labelVar ]             -- property label


-- | The query function `getMetaData` returns all triples with an
-- arm:ViewProperty.  There is no error checking.
-- The given graph must contain one CharacterSheet only.
getMetaData :: G.RDFGraph -> KeyPairList
getMetaData = KeyPairList . toKeyPairList . map arcFromBinding . Q.rdfQueryFind arcs
