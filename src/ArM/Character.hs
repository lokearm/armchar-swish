{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- This module exposes four internal modules.
-- *  `ArM.Character.Character` defines the `CharacterSheet` datatype and
--    the functions to advance them.
-- *  `ArM.Character.Trait` handles processing and advancement of `Trait`
-- *  `ArM.Character.Advancement` handles processing of the `Advancement`
--    type.
-- Most of the actual types are defined in `ArM.Types.Character`
--
-----------------------------------------------------------------------------
module ArM.Character ( module ArM.Character.Character
                     , module ArM.Character.Trait
                     ) where

import ArM.Character.Character 
import ArM.Character.Trait
