{-# LANGUAGE OverloadedStrings #-}

module ArM.BlankNode where

import Swish.RDF.Graph
import Control.Monad.State.Lazy

type BlankState = State (String,Int) 

getBlank :: BlankState RDFLabel
getBlank = do
    (s,x) <- get
    put (s,x+1)
    return $ Blank $ s ++ show x

runBlank = runState
