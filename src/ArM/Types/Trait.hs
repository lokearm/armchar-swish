{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle Characters and Traits, with some basic associated functions.
--
-----------------------------------------------------------------------------
module ArM.Types.Trait where

import Swish.RDF.Graph as G
-- import qualified Swish.RDF.Query as Q
-- import Data.Maybe
import Data.Aeson
-- import ArM.Types.Season
import ArM.KeyPair
import ArM.Resources
-- import ArM.BlankNode
-- import ArM.Rules.Aux
-- import ArM.Types.RDF

-- | 
-- = Trait

-- | Trait Resource
-- `traitID` and `traitContents` are sufficient to describe the trait.
-- The other fields duplicate information to facilitate searching and
-- sorting.
-- When new traits are created, `traitID` is set to nothing?
-- A blank node is only created when it is written into an RDFGraph.
data Trait = Trait {
    traitClass :: RDFLabel,
    instanceLabel :: String,
    isRepeatableTrait :: Bool,
    traitContents :: [RDFTriple]
   } deriving (Eq)
defaultTrait :: Trait
defaultTrait = Trait {
    traitClass = noSuchTrait,
    instanceLabel = "",
    isRepeatableTrait = False,
    traitContents = []
   } 

-- | Get the ID (RDFLabel) of a trait if possible.
traitID :: Trait -> Maybe RDFLabel
traitID = f . traitContents
   where f [] = Nothing
         f (x:_) = Just $ arcSubj x

instance Show Trait where
   show a = "**" ++ show (traitClass a) ++ "**\n" 
                 ++ sc (traitContents a) 
                 ++ "\n"
      where 
         sc [] = ""
         sc (x:xs) = "  " ++ show x ++ "\n" ++ sc xs
instance Ord Trait where
   compare x y | traitClass x < traitClass y = LT
               | traitClass x > traitClass y = GT
               -- | not (isRepeatableTrait x) = EQ
               -- | not (isRepeatableTrait y) = EQ
               | instanceLabel x < instanceLabel y = LT
               | instanceLabel x > instanceLabel y = GT
               | otherwise = EQ

instance ToJSON Trait where 
    toJSON t = toJSON $ KeyPairList $ toKeyPairList $ traitContents t 
instance FromJSON Trait where 
    parseJSON val = do 
                     v <- parseJSON val
                     return $ f1 v
       where f1 (KeyPairList x ) = defaultTrait { traitContents = map f2 x }
             f2 (KeyValuePair x y) = arc (armRes "unnamedBlankNode") x y
