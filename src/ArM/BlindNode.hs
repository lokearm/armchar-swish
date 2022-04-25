{-# LANGUAGE OverloadedStrings #-}

module ArM.BlindNode where

import Swish.RDF.Graph (RDFLabel(..))
import Control.Monad.State.Lazy

type BlindState = State (String,Int) 

getBlind :: BlindState RDFLabel
getBlind = do
    (s,x) <- get
    put (s,x+1)
    return $ Blind $ s ++ show x

runBlind = runState
