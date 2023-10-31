{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.KeyPair
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Query functions and associated data types.
--
-- Data types `KeyValuePair` and `KeyPairList` are used to hold
-- property/value pairs associated with RDF resources and corresponding
-- to the predicate and object from RDF Triples.
--
-- Queries must be constructed for each particular case, but if they
-- use the three variables defined here (`propertyVar`, `idVar`, `valueVar`)
-- the results can be processed using `arcFromBinding` or
-- `keypairFromBinding`.  The latter assumes that all results share the
-- same subject.
--
-----------------------------------------------------------------------------
module ArM.KeyPair ( keypairFromBinding 
                   , keyvalueToArcList 
                   , tripleToJSON 
                   , KeyValuePair(..)
                   , KeyPairList(..)
                   , fromKeyPairList
                   , toKeyPairList
                   , arcFromBinding
                   , arcListSplit
                   , getProperty
                   , getIntProperty
                   , propertyVar
                   , idVar
                   , valueVar
                   , labelVar
                   , metadataFromBinding
                   ) where

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import qualified Swish.RDF.VarBinding as VB 
import Swish.VarBinding  (vbMap)
import Data.Maybe  (fromJust)
import Data.List (sort)
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import ArM.Types.RDF
import ArM.Rules.Aux

import ArM.Debug.NoTrace

-- |
-- = Data Types

-- | `KeyValuePair` represents a key/value pair in JSON jargon
-- or a property/object pair in RDF.  It is designed to hold
-- arbitrary data read from the source format.
data KeyValuePair = KeyValuePair RDFLabel RDFLabel
     deriving (Show,Eq,Ord)

-- | A list of KeyPair objects, typically used for property/object pairs.
-- This is made an algebraic data type to allow class instantiation
-- for JSON and Show compatibility, but it is internally stored as a list.
data KeyPairList  = KeyPairList [KeyValuePair]
    deriving Eq
instance Show KeyPairList where
        show (KeyPairList []) = ""
        show (KeyPairList (x:_)) = "  " ++ show x ++ "\n" 

-- | Get the `KeyPairList` as a list of `KeyValuePair` objects.
fromKeyPairList :: KeyPairList -> [KeyValuePair]
fromKeyPairList (KeyPairList xs) = xs

-- |
-- == Conversion from RDF Triples

-- | Convert a triple to a pair by removing the subject (first term)
toKeyPair :: G.RDFTriple -> KeyValuePair 
toKeyPair x = KeyValuePair (arcPred x) (arcObj x)
-- | Convert a list of triples to a pairs (using `toKeyPair`)
toKeyPairList :: [G.RDFTriple] -> [KeyValuePair]
toKeyPairList = map toKeyPair

-- |
-- = Queries


-- | Variable used for the resource ID in queries.
idVar :: RDFLabel
idVar = (G.Var "id")
-- | Variable used for a property of interest in queries.
propertyVar :: RDFLabel
propertyVar = (G.Var "property")
-- | Variable used for a value associated with the property of interest.
valueVar :: RDFLabel
valueVar = (G.Var "value")
-- | Variable used for a human readable label for the property of interest.
-- This is not really used in this module, but may be used in others.
labelVar :: RDFLabel
labelVar = (G.Var "label")

-- |
-- == Process query results

-- | Map variable bindings to triples of (property,label,value)
-- Three variables should be bound, property, label, and value.
keypairFromBinding :: VB.RDFVarBinding -> KeyValuePair
keypairFromBinding = f . metadataFromBinding 
     where 
       f (p,_,value) = KeyValuePair (fromJust p) (fromJust value) 

-- | Map the variable bindings to Maybe RDFLabel.
-- Auxiliary to `keypairFromBinding`
metadataFromBinding :: VB.RDFVarBinding 
                 -> (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
metadataFromBinding vb = (vbMap vb (G.Var "property"),
                          vbMap vb (G.Var "label"),
                          vbMap vb (G.Var "value"))


-- | Map variable bindings to RDF triples.
-- Three variables should be bound, id, property, and value.
arcFromBinding :: VB.RDFVarBinding -> G.RDFTriple
arcFromBinding vb = G.arc (fromJust $ vbMap vb idVar)
                     (fromJust $ vbMap vb propertyVar)
                     (fromJust $ vbMap vb valueVar)

-- | Split a list of RDF triples so that pairs belonging
-- to the same resource, -- as defined by the first element,
-- are place in the same constituent list.
arcListSplit :: [G.RDFTriple] -> [[G.RDFTriple]]
arcListSplit xs = fst $ arcListSplit' ([],sort xs)

-- | `arcListSplit'` is a mere auxiliary for `arcListSplit'`
arcListSplit' :: ([[G.RDFTriple]], [G.RDFTriple]) 
           -> ([[G.RDFTriple]], [G.RDFTriple]) 
arcListSplit' (xs,[]) = (xs,[])
arcListSplit' ([],y:ys) = arcListSplit' ([[y]],ys)
arcListSplit' ([]:_,_) = error "Empty constituent list in arcListSplit'"
arcListSplit' ((x:xs):xss,y:ys) 
    | arcSubj x == arcSubj y = arcListSplit' ((y:x:xs):xss, ys)
    | otherwise    = arcListSplit' ([y]:(x:xs):xss, ys)

-- |
-- = Scan lists of key/value pairs

-- | Scan a list of Key/Value pairs for a given property and return an
-- integer value.  If the properrty is not found or is not an integer,
-- 0 is returned.
getIntProperty :: RDFLabel -> KeyPairList -> Int
getIntProperty x (KeyPairList xs) = getIntProperty' x xs
getIntProperty' :: RDFLabel -> [KeyValuePair] -> Int
getIntProperty' _ [] = 0
getIntProperty' k' (KeyValuePair k v:xs) 
   | k' == k  = f (fromRDFLabel v)
   | otherwise      = getIntProperty' k' xs
   where f Nothing = 0
         f (Just x) = x

-- | Scan a list of Key/Value pairs for a given property and return the value.
getProperty :: RDFLabel -> [KeyValuePair] -> Maybe RDFLabel
getProperty _ [] = Nothing
getProperty k' (KeyValuePair k v:xs) 
   | k' == k  = Just v
   | otherwise      = getProperty k' xs

-- | Convert a `KeyValuePair` to JSON
tripleToJSON :: KeyValuePair -> (Key,Value)
tripleToJSON (KeyValuePair a b) = 
    ((getKey $ fromJust $ fromRDFLabel a), (toJSON b))


-- | Convert an RDFLabel to an Aeson Key for JSON serialisation
getKey :: RDFLabel -> Key
getKey = fromString  . show

-- |
-- = KeyPairList

instance FromJSON KeyPairList  where
  parseJSON = withObject "KeyPairList" $ \obj ->
    trace ( "parseJSON for KeyPairList " ++ show obj) $
    let kvs = KM.toList obj
        parsed = mapM pairToKeyValue kvs
    in fmap KeyPairList parsed
instance ToJSON KeyPairList where 
    toJSON (KeyPairList t) = object $ map tripleToJSON t

-- | Parse a JSON attribute/value pair and return a KeyValuePair 
-- (property/object in RDF terms)
-- This is a an axiliary for `parseJSON` for `KeyPairList`
-- pairToKeyValue :: (Key,Value) -> Parser KeyValuePair
pairToKeyValue :: (Key,Value) -> Parser KeyValuePair
pairToKeyValue (x,y) = do
    v <- parseJSON y
    case k of
        Left k' -> return $ KeyValuePair k' v
        Right s -> trace "Fail in pairToKeyValue" $ fail s
    where k = stringToRDFLabel $ toString x

-- | Convert `KeyValuePair` to `RDFTriple`
-- This is an auxiliary for other ToRDFGraph functions
keyvalueToArcList :: RDFLabel -> [KeyValuePair] -> [RDFTriple]
keyvalueToArcList _ [] = []
keyvalueToArcList x (KeyValuePair a c:ys) = arc x a c:keyvalueToArcList x ys

instance FromRDFGraph KeyPairList where
   fromRDFGraph g label = KeyPairList $ map keypairFromBinding 
                        $  Q.rdfQueryFind query g
       where query = listToRDFGraph [ arc label (G.Var "property") (G.Var "value") ]

