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
-- import ArM.Query
import ArM.Character
import ArM.KeyPair
import Data.Maybe
import Network.URI (URI)
import qualified Data.Text as T
import Swish.RDF.Vocabulary.XSD


import qualified Data.Aeson.KeyMap as KM


-- We need to rethink the use of triples here, as it is not
-- one-to-one and contains redundant data.
-- Do we need the label from the (property,label,value) triple?

-- We may want to make an algebraic datatype to replace Triple
-- See here for decoding tips
-- https://stackoverflow.com/questions/53478455/aeson-parse-json-object-to-list
--
-- Another comprehensive tutorial:
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html

tripleToJSON (KeyValuePair a b) = tripleToJSON' (fromJust $ fromRDFLabel a) (labelToData b)
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


data KeyPairList  = KeyPairList [KeyValuePair]

instance FromJSON RDFLabel where
   parseJSON (Number x) = return $ TypedLit (T.pack $ show  x) xsdInteger
   parseJSON (String x) = return $ Lit x

-- instance FromJSON KeyPairList  where
  -- parseJSON = withObject "KeyPairList" $ \obj ->
    -- let kvs = KM.toList obj
        -- parsed = mapM pairToKeyValue kvs
    -- in fmap KeyPairList parsed

-- pairToKeyValue (x,y) = do
    -- v <- parseJSON y
    -- return $ KeyValuePair k v
    -- where k = Blank $ toString x

instance ToJSON Trait where 
    toJSON t = object $ map tripleToJSON (traitContents t) 

-- instance FromJSON Trait where 
    -- parseJSON cs = object (c:x:xs)
    -- parseJSON val = withObject "Test"
                               -- (\o -> Test <$> mapM parseJSON (elems o))
                               -- val

instance ToJSON CharacterSheet where 
    toJSON cs = object (c:x:xs)
       where x = (fromString "arm:hasTrait") .= (toJSON (csTraits cs))
             xs = map tripleToJSON (csMetadata cs)
             c = (fromString "arm:isCharacter") .= (show $ csID cs)
    
instance ToJSON Advancement where 
    toJSON cs = object (c:x:xs)
       where x = (fromString "arm:hasTrait") .= (toJSON (traits cs))
             xs = map tripleToJSON (contents cs)
             c = (fromString "id") .= (advancementIDstring cs)

