{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Char.Markdown where

-- import Data.Maybe (fromJust)
import qualified Data.Text as T

import ArM.Char.Character
import ArM.Char.Trait

class Show a => Markdown a where
     printMD :: a -> [ String ]

instance Markdown FieldValue where
   printMD =  (:[]) . show

instance Markdown KeyPair where
   printMD (KeyPair x  y) = [ x, ':':' ':show y, "" ]
instance Markdown KeyPairList where
   printMD (KeyPairList xs) = ( foldl (++) [] $ map printMD xs )
instance Markdown CharacterConcept where
   printMD c = ( printMD $ charGlance c ) ++ ( printMD $ charData c )
instance Markdown Character where
   printMD = printMD . concept 
