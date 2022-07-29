{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Metadata
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Handling character metadata
--
-----------------------------------------------------------------------------

module ArM.Character.Metadata ( getCharacterMetadata, characterFromGraph ) where
 

import ArM.Rules.Aux
import ArM.Resources
import ArM.KeyPair
import ArM.Types.Character

import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import Swish.RDF.VarBinding as VB 
import Swish.VarBinding 
import Data.List (sort)


-- | Construct a query to get all
-- arm:CharacterProperty triples for a given subject.
query c = listToRDFGraph 
   [ arc c (G.Var "property") (G.Var "value")
   , arc (G.Var "property") typeRes armCharacterProperty
   , arc (G.Var "property") labelRes  (G.Var "label") ]

-- | Find all characters in a given graph.  Auxiliary for `characterFromGraph`.
characterFromGraph' :: RDFGraph -> [VB.RDFVarBinding]
characterFromGraph' = Q.rdfQueryFind
             $ listToRDFGraph  [ arc cVar typeRes armCharacter ]
-- | Get the labels of all characters in a given graph.
characterFromGraph :: RDFGraph -> [RDFLabel]
characterFromGraph = uniqueSort . f . map (`vbMap` cVar) . characterFromGraph' 
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs
-- | Sort the list and remove duplicates.
uniqueSort :: (Ord a,Eq a) => [a] -> [a]
uniqueSort = f . sort
    where f [] = []
          f (x:[]) = x:[]
          f (x:y:ys) | x == y = f (y:ys)
          f (x:y:ys) | x /= y = x:f (y:ys)

-- | Make a list of metadata, where each data item is
-- a triple consisting of URI, Label, and Value.
-- The inputs are an 'RDFGraph' g and a string naming an RDF resource,
-- either as a prefixed name or as a full URI in angled brackets (<uri>).
getCharacterMetadata :: G.RDFGraph -> RDFLabel -> KeyPairList
getCharacterMetadata g s = KeyPairList $ map keypairFromBinding
                          $  getCharacterMetadataVB g s

-- | Get the variable bindings from the graph.
getCharacterMetadataVB :: G.RDFGraph -> RDFLabel -> [VB.RDFVarBinding]
getCharacterMetadataVB g c = Q.rdfQueryFind (query c) g


instance FromRDFGraph Character where
   fromRDFGraph g label = defaultCharacter {
                 characterID = label,
                 characterData = getCharacterMetadata g label
                 }
