{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Handling character advancement
--
-----------------------------------------------------------------------------

module ArM.Character.Advancement ( getPregameAdvancements
                                 , getIngameAdvancements
                                 , getAllAdvancements
                                 ) where

import Swish.RDF.Graph 
import Swish.RDF.Query as Q
import ArM.Resources 
import ArM.KeyPair 
-- import ArM.Character.Trait
import ArM.Types.Advancement
-- import ArM.Types.Character
import ArM.Types.Season
import ArM.Rules.Aux

-- | Get a list of all Pregame Advancements of a character.
getPregameAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getPregameAdvancements g c = getAdvancements g $ queryGraph preGameAdv c
   where preGameAdv = armRes  "PregameAdvancement"

-- | Get a list of all Ingame Advancements of a character.
getIngameAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getIngameAdvancements g c = getAdvancements g $ queryGraph inGameAdv c
   where inGameAdv = armRes  "IngameAdvancement"

getAllAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getAllAdvancements g c = getAdvancements g $ queryGraph inGameAdv c
   where inGameAdv = armRes  "CharacterAdvancement"

-- | Query graph to find a advancements of a given type (RDF class)
-- for a given character.
queryGraph :: RDFLabel -- ^ Label for the advancement type
           -> RDFLabel -- ^ Label for the character to be advanced
           -> RDFGraph -- ^ Resulting graph
queryGraph c = listToRDFGraph  . f c
   where f c1 c2 = [ arc (Var "id") typeRes c1,
            arc (Var "id") (Var "property") (Var "value"),
            arc (Var "id") (armRes  "advanceCharacter") c2,
            arc (Var "property") labelRes (Var "label") ]

-- | Generic version of 'getIngameAdvancements' and 'getPregameAdvancements'
getAdvancements :: RDFGraph -> RDFGraph -> [Advancement]
getAdvancements g = fixAdvancements g . 
               map toAdvancement . arcListSplit . getGenQuads g 

-- | Auxiliary for 'getAdvancements'
getGenQuads :: RDFGraph -> RDFGraph -> [RDFTriple]
getGenQuads g q = map arcFromBinding $ rdfQueryFind q g

-- | Auxiliary for `getAdvancements`
fixAdvancements :: RDFGraph -> [Advancement] -> [Advancement]
fixAdvancements g adv = map (fixAdv g) adv

-- | Make an Advancement object from a list of Quads
toAdvancement :: [RDFTriple] -> Advancement
toAdvancement xs = defaultAdvancement { rdfid = getkey xs
                                      , advTime = parseTime defaultCharTime ys 
                                      , contents = ys }
         where ys = toKeyPairList xs 
               getkey [] = noSuchAdvancement
               getkey (x:_) = arcSubj x

