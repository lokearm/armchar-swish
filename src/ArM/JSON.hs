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
import Swish.RDF.Graph (RDFLabel(..), fromRDFLabel)
import Swish.Namespace (ScopedName)
import ArM.Character
import ArM.KeyPair
import ArM.Resources
import Data.Maybe  (fromJust)
import Network.URI (URI)
import qualified Data.Text as T
import Swish.RDF.Vocabulary.XSD (xsdInteger)
import Data.List       (intercalate)
import Data.List.Split (splitOn)


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

tripleToJSON (KeyValuePair a b) = 
    ((getKey $ fromJust $ fromRDFLabel a), (toJSON b))

getKey :: RDFLabel -> Key
getKey = fromString  . show

labelToData :: RDFLabel -> Either KVP (Either Int String)
labelToData l | i /= Nothing = Right $ Left (fromJust i)
              | s /= Nothing = Right $ Right (fromJust s)
              | uri /= Nothing = Left $ KVP (show $ fromJust uri)
              | otherwise    = Right (Right (show l))
    where  s = fromRDFLabel l :: Maybe String
           i = fromRDFLabel l :: Maybe Int
           uri = fromRDFLabel l :: Maybe ScopedName


data KeyPairList  = KeyPairList [KeyValuePair]

data KVP = KVP { prefixedid :: String }
   deriving (Show,Eq)
instance ToJSON KVP where 
    toJSON k = object [ fromString "prefixedid" .= toJSON ( prefixedid k ) ]
instance FromJSON KVP where 
    parseJSON (Object x) = KVP <$> x .: "prefixedid"

instance ToJSON RDFLabel where
    toJSON  = f . labelToData
        where f (Left x) = toJSON x
              f (Right (Left x)) = toJSON x
              f (Right (Right x)) = toJSON x

instance FromJSON RDFLabel where
   parseJSON (Number x) = return $ TypedLit (T.pack $ show  x) xsdInteger
   parseJSON (String x) = return $ Lit x
   parseJSON x = fmap (stringToRDFLabel . prefixedid) $ parseJSON x

stringToRDFLabel k = f x $ intercalate "" xs
     where (x:xs) = splitOn ":" $ k
           f "arm" = Res . makeSN 
           f "armchar" = armcharRes
           f "armr" = armrRes

instance FromJSON KeyPairList  where
  parseJSON = withObject "KeyPairList" $ \obj ->
    let kvs = KM.toList obj
        parsed = mapM pairToKeyValue kvs
    in fmap KeyPairList parsed
instance ToJSON KeyPairList where 
    toJSON (KeyPairList t) = object $ map tripleToJSON t

pairToKeyValue (x,y) = do
    v <- parseJSON y
    return $ KeyValuePair k v
    where k = stringToRDFLabel $ toString x


instance ToJSON Trait where 
    toJSON t = toJSON $ KeyPairList $ f (traitID t)
        where f Nothing = t'
              f (Just x) = KeyValuePair prefixedidRes x:t'
              t' = traitContents t 
instance FromJSON Trait where 
    parseJSON val = do 
                     v <- parseJSON val
                     return $ kpToTrait' v
       where kpToTrait' (KeyPairList x ) = kpToTrait x


instance ToJSON CharacterSheet where 
    toJSON cs = object (c:x:xs)
       where x = (fromString "arm:hasTrait") .= (toJSON (csTraits cs))
             xs = map tripleToJSON (csMetadata cs)
             c = (fromString "arm:isCharacter") .= (show $ csID cs)

-- TODO  
-- instance FromJSON CharacterSheet where 

data ProtoAdvancement = ProtoAdvancement {
    advancementid :: RDFLabel,
    advancementcontents :: KeyPairList,
    advancementtraits :: [Trait]
   } 

instance ToJSON Advancement where 
    toJSON cs = object (c:x:y:[])
       where x = (fromString "advancementtraits") .= (toJSON (traits cs))
             y = (fromString "advancementcontents") .= KeyPairList (contents cs)
             c = (fromString "advancementid") .= toJSON (rdfid cs)

-- TODO  
instance FromJSON Advancement where 
   parseJSON = fmap fromProtoAdvancement . parseJSON
instance FromJSON ProtoAdvancement where 
   parseJSON (Object v) = ProtoAdvancement <$> v .: "advancementid"
                                           <*> v .: "advancementcontents"
                                           <*> v .: "advancementtraits"
fromKPL (KeyPairList x ) = x
fromProtoAdvancement :: ProtoAdvancement -> Advancement
fromProtoAdvancement adv = defaultAdvancement {
                     rdfid = advancementid adv,
                     traits = advancementtraits adv,
                     year = getYear ys,
                     season = getSeason ys,
                     advSortIndex = getSortIndex ys,
                     contents = ys
                 } where ys = fromKPL $ advancementcontents adv
