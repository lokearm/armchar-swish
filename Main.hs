module Main where

import System.IO
import Control.Monad

import Swish.RDF.Ruleset

main :: IO ()
main = do
        let list = []
        handle <- openFile "Ontology/cieran.ttl" ReadMode
        contents <- hGetContents handle
        print contents
        hClose handle

f :: [String] -> [Int]
f = map read

