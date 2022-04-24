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
