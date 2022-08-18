{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.JSON
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Functions to output and parse JSON data.  The module defines
-- instances of `ToJSON` and `FromJSON`.
--
-----------------------------------------------------------------------------
module ArM.JSON where

import Data.Scientific
import Control.Monad.Fail

import Control.Applicative
import Data.Aeson
import Data.Aeson.Key
import Swish.RDF.Graph (RDFLabel(..), fromRDFLabel, toRDFLabel, arc)
import Swish.Namespace (ScopedName)
import ArM.Types.Character
import ArM.Character
import ArM.KeyPair
import ArM.Resources
import Data.Maybe  (fromJust)
import Network.URI (URI,parseURI)
import qualified Data.Text as T
import Swish.RDF.Vocabulary.XSD (xsdInteger)
import Data.List       (intercalate)
import Data.List.Split (splitOn)

import qualified Data.Aeson.KeyMap as KM

-- Some good ideas from the web:
-- https://stackoverflow.com/questions/53478455/aeson-parse-json-object-to-list
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html

-- | Convert a `KeyValuePair` to JSON
tripleToJSON :: KeyValuePair -> (Key,Value)
tripleToJSON (KeyValuePair a b) = 
    ((getKey $ fromJust $ fromRDFLabel a), (toJSON b))

-- | Convert a string to an RDFLabel, parsing URIs
stringToRDFLabel :: String -> Either RDFLabel String
stringToRDFLabel (k:ks) 
        | k == '<'  = tl $ parseURI uri 
        | splits == [] = Right "Parser error: empty string."
        | splits' == [] = Right "Parser error: neither prefixed ID nor full URI."
        | px == Nothing = Right "Parse error: Prefix not recognised"
        | otherwise = Left $ fromJust px
     where splits = splitOn ":" $ (k:ks)
           splits' = tail splits
           (x:xs) = splits
           f "arm" = Just . armRes
           f "armchar" = Just . armcharRes
           f "armr" = Just . armrRes
           f _ = \ _ -> Nothing
           px =  f x $ intercalate ":" xs
           rdfuri ('>':[]) = []
           rdfuri (_:[]) = error "Malformed URL in RDF.  No closing >."
           rdfuri (x:xs) = x:rdfuri xs
           uri = rdfuri ks
           tl Nothing = Right "Parser error: Could not parse URI."
           tl (Just x) = Left $ toRDFLabel x

-- | Convert an RDFLabel to an Aeson Key for JSON serialisation
getKey :: RDFLabel -> Key
getKey = fromString  . show

-- | Convert an RDFLabel to String/Int/URI for serialisation purposes.
labelToData :: RDFLabel -> Either KVP (Either Int String)
labelToData l | i /= Nothing = Right $ Left (fromJust i)
              | s /= Nothing = Right $ Right (fromJust s)
              | uri /= Nothing = Left $ KVP (show $ fromJust uri)
              | otherwise    = Right (Right (show l))
    where  s = fromRDFLabel l :: Maybe String
           i = fromRDFLabel l :: Maybe Int
           uri = fromRDFLabel l :: Maybe ScopedName

-- | A `KVP` holds a prefixed ID for RDF serialisation.
-- In JSON this appears as an object with only the `prefixedid` attribute.
data KVP = KVP { prefixedid :: String }
   deriving (Show,Eq)
instance ToJSON KVP where 
    toJSON k = object [ fromString "prefixedid" .= toJSON ( prefixedid k ) ]
instance FromJSON KVP where 
    parseJSON (Object x) = KVP <$> x .: "prefixedid"

-- ** RDFLabel

instance ToJSON RDFLabel where
    toJSON  = f . labelToData
        where f (Left x) = toJSON x
              f (Right (Left x)) = toJSON x
              f (Right (Right x)) = toJSON x
instance FromJSON RDFLabel where
   parseJSON (Number x) = return $ TypedLit (T.pack $ show  (truncate x::Int)) xsdInteger
   parseJSON (String x) = return $ Lit x
   parseJSON x = do
       s <- fmap prefixedid $ parseJSON x
       case stringToRDFLabel s of
          Left x -> return x
          Right x -> fail x



-- ** KeyPairList

instance FromJSON KeyPairList  where
  parseJSON = withObject "KeyPairList" $ \obj ->
    let kvs = KM.toList obj
        parsed = mapM pairToKeyValue kvs
    in fmap KeyPairList parsed
instance ToJSON KeyPairList where 
    toJSON (KeyPairList t) = object $ map tripleToJSON t

-- | Parse a JSON attribute/value pair and return a KeyValuePair 
-- (property/object in RDF terms)
-- This is a an axiliary for `parseJSON` for `KeyPairList`
pairToKeyValue (x,y) = do
    v <- parseJSON y
    case k of
        Left k' -> return $ KeyValuePair k' v
        Right s -> fail s
    where k = stringToRDFLabel $ toString x

-- ** Trait

instance ToJSON Trait where 
    toJSON t = toJSON $ KeyPairList $ f (traitID t)
        where f Nothing = t'
              f (Just x) = KeyValuePair prefixedidRes x:t'
              t' = toKeyPairList $ traitContents t 
instance FromJSON Trait where 
    parseJSON val = do 
                     v <- parseJSON val
                     return $ f1 v
       where f1 (KeyPairList x ) = defaultTrait { traitContents = map f2 x }
             f2 (KeyValuePair x y) = arc (armRes "unnamedBlankNode") x y

-- ** CharacterSheet

instance ToJSON CharacterSheet where 
    toJSON cs = object (c:x:xs)
       where x = (fromString "arm:hasTrait") .= (toJSON (csTraits cs))
             xs = map tripleToJSON (fromKeyPairList $ csMetadata cs)
             c = (fromString "arm:isCharacter") .= (show $ csID cs)

instance ToJSON Character where 
    toJSON c = toJSON $ p x xs
        where x = KeyValuePair (armRes "isCharacter") $ characterID c
              xs = characterData c 
              p x (KeyPairList xs) = KeyPairList (x:xs) 

instance FromJSON Character where 
    parseJSON val = fmap kpToChar $ parseJSON val

-- | Auxiliary to parseJSON Character
kpToChar :: KeyPairList -> Character
kpToChar (KeyPairList xs) = defaultCharacter {
         characterID = fromJ $ getProperty (armRes "isCharacter") xs,
         characterData = KeyPairList xs
         }
         where fromJ Nothing = noSuchCharacter
               fromJ (Just x) = x

-- |
-- = Advancement

data ProtoAdvancement = ProtoAdvancement {
    advancementid :: RDFLabel,
    advancementcontents :: KeyPairList,
    advancementtraits :: [Trait],
    advancementitems :: [Trait]
   } 

instance ToJSON Advancement where 
    toJSON cs = object (c:x:z:y:[])
       where x = (fromString "advancementtraits") .= (toJSON (traits cs))
             z = (fromString "advancementitems") .= (toJSON (items cs))
             y = (fromString "advancementcontents") .= KeyPairList (contents cs)
             c = (fromString "advancementid") .= toJSON (rdfid cs)

instance FromJSON Advancement where 
   parseJSON = fmap fromProtoAdvancement . parseJSON
instance FromJSON ProtoAdvancement where 
   parseJSON (Object v) = ProtoAdvancement <$> v .: "advancementid"
                                           <*> v .: "advancementcontents"
                                           <*> v .: "advancementtraits"
                                           <*> v .: "advancementitems"
fromProtoAdvancement :: ProtoAdvancement -> Advancement
fromProtoAdvancement adv = defaultAdvancement {
                     rdfid = advancementid adv,
                     traits = advancementtraits adv,
                     items = advancementitems adv,
                     year = getYear ys,
                     season = getSeason ys,
                     advSortIndex = getSortIndex ys,
                     contents = ys
                 } where ys = fromKeyPairList $ advancementcontents adv
