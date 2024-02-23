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
import ArM.Types.Season
import ArM.Types.LoadedSaga
import ArM.Types.Advancement
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
writeAdv (Just fn) cg = write fn $ "# Advancement Log":printAdvancementLog  as
     where as = filterAdv True . map TCG.advancement $ TCG.charSheets cg

-- | Write the advancement log of the given character to the given file
-- If the filename is Nothing, nothing is done
writeGen :: Maybe String  -- ^ filename
         -> TCG.CharGen   -- ^ Character object
         -> IO ()
writeGen Nothing _ = return ()
writeGen (Just fn) cg = write fn $ "# Character Generation":printAdvancementLog  as
     where as = filterAdv False  . map TCG.advancement $ TCG.charSheets cg

filterAdv :: Bool -> [Advancement] -> [Advancement]
filterAdv _  []  = []
filterAdv True (x:xs) | advYear x == Nothing = filterAdv True xs 
filterAdv True (x:xs) | otherwise = x:filterAdv True xs 
filterAdv False (x:xs) | advYear x == Nothing = x:filterAdv False xs 
filterAdv False (_:xs) | otherwise = filterAdv False xs 

advYear :: Advancement -> Maybe Int
advYear = charYear . advTime 

-- | Write the saga and covenant to the given file
writeSaga :: String       -- ^ Filename
          -> LoadedSaga   -- ^ Saga 
          -> IO ()
writeSaga fn sob = write fn [ '#':' ':sagaTitle sob ]

-- | Write a character sheet to a markdown file
-- The filename is derived from the source turtle file as stored in the
-- `CharGen` object.
writeCG :: TCG.CharGen -> IO ()
writeCG cg = trace ("Writing " ++ fn) $ writeSheet fn g
     >> writeAdv fn2 cg
     >> writeGen fn4 cg
     >> writeDebug fn3 g
     where g = (TCG.sheetGraph . head . TCG.charSheets) cg
           fn = TCG.charFile cg ++ ".md"
           fn3 = TCG.charFile cg ++ ".triples"
           fn2 = Just $ TCG.charFile cg ++ "-advancement.md"
           fn4 = Just $ TCG.charFile cg ++ "-chargen.md"

writeDebug :: String -> RDFGraph -> IO ()
writeDebug fn cs = do
     h2 <- openFile fn WriteMode
     hPutStrLn h2 $ show cs
     hClose h2


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
