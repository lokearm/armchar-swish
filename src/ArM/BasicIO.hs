-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Simple utilities to read and write files.
--
-- This module contains types to process characters, including 
-- persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.BasicIO where

import System.IO as IO -- for file IO
import Data.Aeson
import Data.Aeson.Text
import Data.Text.Lazy.IO as I


-- | Write a list of strings into the given file
writeLns :: String     -- ^ File name
           -> [ String ] -- ^ Contents
           -> IO ()
writeLns fn c = do
     handle <- openFile fn WriteMode
     let p = IO.hPutStrLn handle
     mapM_ p c
     hClose handle

writeMaybeFile :: Maybe String     -- ^ File name
           -> [ String ]           -- ^ Contents
           -> IO ()
writeMaybeFile Nothing _ = return ()
writeMaybeFile (Just x) y =  writeLns x y

writeMaybeJSON :: ToJSON t => Maybe String -> t -> IO ()
writeMaybeJSON Nothing _ = return ()
writeMaybeJSON (Just fn) y' = I.writeFile fn y
   where y = encodeToLazyText y'

putStrLns :: [ String ] -> IO ()
putStrLns [] = return ()
putStrLns (x:xs) = IO.putStrLn x >> putStrLns xs
