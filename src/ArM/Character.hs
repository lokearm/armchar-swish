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
-- *  `ArM.Character.Metadata` handles `Character` objects, that is the
--    metadata which do not change during the game
-- *  `ArM.Character.Advancement` handles processing of the `Advancement`
--    type.
-- Most of the actual types are defined in `ArM.Types.Character`
--
-----------------------------------------------------------------------------
module ArM.Character ( module ArM.Character.Character
                     , module ArM.Character.Metadata
                     , module ArM.Character.Trait
                     , module ArM.Character.Advancement
                     ) where

import ArM.Character.Character 
import ArM.Character.Metadata
import ArM.Character.Trait
import ArM.Character.Advancement
