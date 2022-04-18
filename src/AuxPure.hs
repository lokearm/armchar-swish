module AuxPure where

import Swish.RDF.Parser.N3 (parseN3fromText)

qparse = either error id . parseN3fromText
