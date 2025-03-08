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
import ArM.Char.Internal.Character
import ArM.Char.Internal.Advancement
import ArM.Char.Advancement
import Data.Maybe

getParam :: Character -> [ VF ] -> CharGenParameters 
getParam c _ | isGrog c = p { vfLimit = 3 }
             | otherwise = p
             where p = defaultParam

getInitVF :: Character -> [ VF ]
getInitVF = getInitVF' . head . pregameAdvancement

getInitVF' :: Advancement -> [ VF ]
getInitVF' a | m /= "Virtues and Flaws" = []
            | otherwise = getVF $ changes a
           where m = fromMaybe "" $ mode a
  

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
