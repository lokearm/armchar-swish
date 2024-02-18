{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.IO
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- IO functions for Markdown.
--
-----------------------------------------------------------------------------
module ArM.Markdown.IO where


import System.IO -- for file IO

import ArM.Debug.Trace

import qualified ArM.Types.CharGen as TCG
import ArM.Types.Saga
import ArM.Sheet.SheetObject
import ArM.Markdown.CharacterSheet
import ArM.Markdown.AdvancementLog
import Swish.RDF.Graph (RDFGraph)

-- | Write a CharacterSheet from an RDFGraph into the given file
writeSheet :: String   -- ^ File name
           -> RDFGraph -- ^ Graph representing the character
           -> IO ()
writeSheet fn cg = do
     handle <- openFile fn WriteMode
     let p = hPutStrLn handle
     mapM_ p $ printSheetObject  $ getSheetObject cg
     hClose handle

-- | Write the advancement log of the given character to the given file
-- If the filename is Nothing, nothing is done
writeAdv :: Maybe String  -- ^ filename
         -> TCG.CharGen   -- ^ Character object
         -> IO ()
writeAdv Nothing _ = return ()
writeAdv (Just fn) cg = trace fn $ do
     write fn $ "# Advancement Log":printAdvancementLog  as
     where as = map TCG.advancement $ TCG.charSheets cg

-- | Write the saga and covenant to the given file
writeSaga :: String       -- ^ Filename
          -> Saga         -- ^ Saga 
          -> IO ()
writeSaga fn saga = write fn [ '#':' ':sagaTitle saga ]

-- | Write a character sheet to a markdown file
-- The filename is derived from the source turtle file as stored in the
-- `CharGen` object.
writeCG :: TCG.CharGen -> IO ()
writeCG cg = trace ("Writing " ++ fn) $ writeSheet fn g
     >> writeAdv fn2 cg
     where g = (TCG.sheetGraph . head . TCG.charSheets) cg
           fn = TCG.charFile cg ++ ".md"
           fn2 = Just $ TCG.charFile cg ++ "-advancement.md"

-- | Write the Covenant to the given file
writeCovenant :: String -- ^ filename
         -> TCG.CharGen -- ^ CharGen object for the Covenant
         -> IO ()
writeCovenant fn = w . TCG.charSheets
   where w [] = return ()
         w (x:_) = write fn $ printCovenantSheet $ getSheetObject $ TCG.sheetGraph x

-- | Write a list of Strings to the given file
write :: String -- ^ filename
      -> [String]    -- ^ list of lines to write
      -> IO ()
write fn x = do
               handle <- openFile fn WriteMode
               mapM_ (hPutStrLn handle) x
               hClose handle
