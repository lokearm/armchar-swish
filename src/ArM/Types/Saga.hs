{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types and basic functions to handle sagas, that is the
-- top level object of the data model
--
-----------------------------------------------------------------------------
module ArM.Types.Saga where

data Saga = Saga { sagaTitle :: String
                 , schemaFile :: String
                 , resourceFiles :: [String]
                 , characterFiles :: [String]
                 }

defaultSaga = Saga { sagaTitle = "No Title"
                 , schemaFile = "/dev/null"
                 , resourceFiles = []
                 , characterFiles = []
                 }
