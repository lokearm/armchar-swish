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

import Control.Applicative
import Data.Aeson
import Data.Aeson.Key
import Swish.RDF.Graph
import Swish.Namespace
import ArM.Query
import ArM.Advancement (Advancement(..),advancementIDstring)
import ArM.Character
import ArM.Internal.Trait
import Data.Maybe
import Network.URI (URI)
import qualified Data.Text as T

-- We need to rethink the use of triples here, as it is not
-- one-to-one and contains redundant data.
-- Do we need the label from the (property,label,value) triple?

-- We may want to make an algebraic datatype to replace Triple
-- See here for decoding tips
-- https://stackoverflow.com/questions/53478455/aeson-parse-json-object-to-list

tripleToJSON (a,_,b) = tripleToJSON' (fromJust $ fromRDFLabel a) (labelToData b)
tripleToJSON' a (Left b) = (getKey a, Number $ fromIntegral b) 
tripleToJSON' a (Right b) = (getKey a, String $ T.pack b) 

getKey :: RDFLabel -> Key
getKey = fromString  . show

labelToData :: RDFLabel -> Either Int String
labelToData l | i /= Nothing = Left (fromJust i)
              | s /= Nothing = Right (fromJust s)
              | uri /= Nothing = Right (show $ fromJust uri)
              | otherwise    = Right (show l)
    where  s = fromRDFLabel l :: Maybe String
           i = fromRDFLabel l :: Maybe Int
           uri = fromRDFLabel l :: Maybe ScopedName


instance ToJSON Trait where 
    toJSON t = object $ map tripleToJSON (traitContents t) 

-- instance FromJSON Trait where 
    -- parseJSON cs = object (c:x:xs)

instance ToJSON CharacterSheet where 
    toJSON cs = object (c:x:xs)
       where x = (fromString "arm:hasTrait") .= (toJSON (csTraits cs))
             xs = map tripleToJSON (csMetadata cs)
             c = (fromString "arm:isCharacter") .= (csID cs)
    
instance ToJSON Advancement where 
    toJSON cs = object (c:x:xs)
       where x = (fromString "arm:hasTrait") .= (toJSON (traits cs))
             xs = map tripleToJSON (contents cs)
             c = (fromString "id") .= (advancementIDstring cs)

