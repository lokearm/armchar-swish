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
import qualified ArM.Types.Season as TS
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


-- import Debug.Trace
trace x y = y

-- Some good ideas from the web:
-- https://stackoverflow.com/questions/53478455/aeson-parse-json-object-to-list
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html




