-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.CharGen
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Character Generation Management
--
-----------------------------------------------------------------------------
module ArM.Char.CharGen where

import ArM.Char.Trait
import ArM.Char.Character

getParam :: Character -> [ VF ] -> CharGenParameters 
getParam _ _ = defaultParam
  

data CharGenParameters = CharGenParameters 
     { vfLimit :: Int
     , charLimit :: Int
     , ecLimit :: Int
     , apprenticeshipLimit :: Int
     }

defaultParam :: CharGenParameters 
defaultParam = CharGenParameters 
     { vfLimit = 10
     , charLimit = 7 
     , ecLimit = 45
     , apprenticeshipLimit = 240
     }
