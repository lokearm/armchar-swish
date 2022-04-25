{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- The CharacterSheet data type and corresponding functions
--
-----------------------------------------------------------------------------
module ArM.Character where

import Data.Set (fromList)
import Swish.RDF.Parser.N3 (parseN3fromText)
import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import qualified Data.Text.Lazy as T
import Network.URI (URI,parseURI)
import Data.Maybe (fromJust)
import Data.List (sort)

import ArM.Resources
import ArM.KeyPair 
import ArM.BlankNode
import ArM.Rules.Aux

import qualified ArM.Character.Character as CC
import qualified ArM.Character.Metadata as CM
import qualified ArM.Character.Trait as CT
import qualified ArM.Character.Advancement as CA
