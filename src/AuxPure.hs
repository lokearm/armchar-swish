module AuxPure where

import Swish.RDF.Parser.N3 (parseN3fromText)
import Swish.RDF.Graph (RDFGraph)
import Data.Text.Lazy as T

qparse :: String -> RDFGraph
qparse = either error id . parseN3fromText . T.pack
