module Main where

import System.IO as IO
import Data.Text.IO as DTIO
-- import Data.Text.Lazy.IO as DTLIO
import Control.Monad
import Swish.RDF.Formatter.Turtle
import qualified Data.Text.Lazy as  T
-- import Swish.Rule
import Swish.RDF.Graph


import Rules
import AuxIO
import Resources

import Swish.RDF.Ruleset





main :: IO ()
main = do
        let list = []
        character <- readGraph characterFile 
        armGraph <- readGraph armFile 
        resourceGraph <- readGraph resourceFile 
        DTIO.putStrLn $ formatGraphAsText character
        let cs  = fwdApplySimple csRule character
        DTIO.putStrLn $ formatGraphAsText $ head cs
        let m  = fwdApplyMerge csRule character
        print "====="
        print m
        print "====="
        DTIO.putStrLn $ formatGraphAsText $ m

