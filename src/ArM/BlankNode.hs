{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.BlankNode
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- This module defines a monad to create blank nodes with unique IDs.
--
-----------------------------------------------------------------------------

module ArM.BlankNode where

import Swish.RDF.Graph (RDFLabel(..))
import Control.Monad.State.Lazy

type BlankState = State (String,Int) 

getBlank :: BlankState RDFLabel
getBlank = do
    (s,x) <- get
    put (s,x+1)
    return $ Blank $ s ++ show x

runBlank = runState
