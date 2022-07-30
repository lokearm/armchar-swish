{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Common
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Some rules which are used at many different stages of the reasoning.
--
-----------------------------------------------------------------------------

module ArM.Rules.Common where

import Swish.RDF.Graph
import ArM.Resources
import ArM.Rules.Aux

import Control.Parallel

traitclasstypeRule = makeCRule "traitclasstypeRule" 
       [ arc sVar ( armRes "traitClass" ) tVar ]
       [ arc sVar typeRes tVar ]

