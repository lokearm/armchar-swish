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

module ArM.BlankNode ( BlankState
                     , fixBlanksM
                     , runBlank
                     , getBlank 
                     ) where

import Swish.RDF.Graph (RDFLabel(..),RDFTriple,arc,arcObj,arcPred)
import Control.Monad.State.Lazy
import ArM.Types.Trait
import ArM.Resources
import Data.Maybe (fromJust)

type BlankState = State (String,Int) 

getBlank :: BlankState RDFLabel
getBlank = do
    (s,x) <- get
    put (s,x+1)
    return $ Blank $ s ++ show x

runBlank :: BlankState a -> (String,Int) -> (a, (String,Int))
runBlank = runState

replaceBlank :: RDFLabel -> RDFTriple -> RDFTriple
replaceBlank b x =  arc b ( arcPred x ) ( arcObj x )

fixBlanksM :: [Trait] -> BlankState [Trait]
fixBlanksM [] = return []
fixBlanksM (x:xs) = do
             x' <- fixBlankNodeM x
             xs' <- fixBlanksM xs
             return $ x':xs'
fixBlankNodeM :: Trait -> BlankState Trait
fixBlankNodeM t 
   | traitContents t == [] = return t
   | key /= (armRes "unnamedBlankNode") = return t
   | otherwise = do
        b <- getBlank
        return $ t { traitContents = map ( replaceBlank b ) 
                      $ traitContents t }
     where key = fromJust $ traitID t
