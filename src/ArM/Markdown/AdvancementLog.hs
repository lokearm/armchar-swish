{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.CharGen
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle characters as stored in web server memory.
--
-----------------------------------------------------------------------------
module ArM.Markdown.AdvancementLog ( printAdvancementLog ) where


import ArM.Types.Advancement
import Data.List(intercalate)


printAdvancementLog :: [Advancement] -> [String]
printAdvancementLog = intercalate [] . map printAdvancement

printAdvancement :: Advancement -> [String]
printAdvancement ad = [ "+ " ++ show $ advTime ad ]

