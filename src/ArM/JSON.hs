{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.JSON
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Functions to produce 
--
-----------------------------------------------------------------------------
module ArM.JSON where

import Data.Aeson
import Swish.RDF.Graph
import ArM.Query
import ArM.Character
import ArM.Internal.Trait
import Data.Maybe

tripleToJSON (a,_,b) = tripleToJSON' (fromJust $ fromRDFLabel a) (labelToData b)
tripleToJSON' a (Left b) = getKey a .= b 
tripleToJSON' a (Right b) = getKey a .= b 

getKey :: RDFLabel -> Key
getKey = read . show

labelToData :: RDFLabel -> Either Int String
labelToData l | s == Nothing = Left (fromJust i)
              | otherwise    = Right (fromJust s)
    where  s = fromRDFLabel l :: Maybe String
           i = fromRDFLabel l :: Maybe Int


instance ToJSON Trait where 
    toJSON t = object $ map tripleToJSON (traitContents t) 

instance ToJSON CharacterSheet where 
    toJSON cs = object (x:xs)
       where x = (read "arm:hasTrait") .= (toJSON (csTraits cs))
             xs = map tripleToJSON (csMetadata cs)
    
