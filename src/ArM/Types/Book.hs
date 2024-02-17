{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Book
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Types.Book ( Book(..)
                      , parseBook
                       ) where

import Swish.RDF.Graph as G
import ArM.KeyPair
import ArM.Resources
import ArM.Types.RDF()

import Data.Maybe (fromJust)

-- | 
-- = Trait

-- | Trait Resource
-- `traitID` and `traitContents` are sufficient to describe the trait.
-- The other fields duplicate information to facilitate searching and
-- sorting.
-- When new traits are created, `traitID` is set to nothing?
-- A blank node is only created when it is written into an RDFGraph.
data Book = Book {
    bookTrait :: Maybe String,
    bookLevel :: Maybe Int,
    bookQuality :: Maybe Int,
    bookAuthor :: Maybe String,
    bookTitle :: Maybe String,
    bookQuantity :: Maybe Int
   } deriving (Ord,Eq)
defaultBook :: Book
defaultBook = Book {
    bookTrait = Nothing,
    bookLevel = Nothing,
    bookQuality = Nothing,
    bookAuthor = Nothing,
    bookTitle = Nothing,
    bookQuantity = Nothing
   } 

bTitle :: Book -> String
bTitle x | bookTitle x == Nothing = "No title "
         | otherwise    = "*" ++ (fromJust $ bookTitle x) ++ "* "
byLine :: Book -> String
byLine x | bookAuthor x == Nothing = ""
         | otherwise = "by " ++ (fromJust $ bookAuthor x) ++ " "
bTopic :: Book -> String
bTopic x | bookTrait x == Nothing = "on unspecified topic "
         | otherwise   = "on " ++ (fromJust $ bookTrait x) ++ " "
bQ :: Book -> String
bQ x | bookTrait x == Nothing = "(one copy) "
     | otherwise   = "(" ++ (show $ fromJust $ bookQuantity x) ++ " copies) "

instance Show Book where
   show b = bTitle b ++ byLine b
          ++ bTopic b ++ lq ++ bQ b
      where lq = lqString (bookLevel b) (bookQuality b)

lqString :: Maybe Int -> Maybe Int -> String
lqString Nothing  Nothing = " "
lqString (Just x)  Nothing = "(L" ++ show x ++ "Q??) "
lqString (Just x)  (Just y) = "(L" ++ show x ++ "Q" ++ show y ++ ") "
lqString Nothing  (Just y) = "(Q" ++ show y ++ ") "

parseBook :: KeyPairList -> Book
parseBook (KeyPairList pl) = parseBook' pl defaultBook 
parseBook' :: [KeyValuePair] -> Book -> Book
parseBook' [] t = t
parseBook' (x:xs) t = parseBook' xs (parsePair x t)

parsePair :: KeyValuePair -> Book -> Book
parsePair (KeyValuePair res  x) t 
    | res == (armRes "hasTrait")      = t { bookTrait = G.fromRDFLabel x }
    | res == (armRes "hasLevel")      = t { bookLevel = G.fromRDFLabel x }
    | res == (armRes "hasQuality")      = t { bookQuality = G.fromRDFLabel x }
    | res == (armRes "hasAuthorString") = t { bookAuthor = G.fromRDFLabel x }
    | res == (armRes "hasTitle")      = t { bookTitle = G.fromRDFLabel x }
    | res == (armRes "hasQuantity")      = t { bookQuantity = G.fromRDFLabel x }
    | otherwise = t 
