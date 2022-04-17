module Rules where

import Swish.RDF.Ruleset
import Swish.Namespace
import qualified Swish.QName as QN
import Network.URI
import qualified Data.Text as T
import Data.Text.Lazy.Builder




auth = URIAuth "" "hg.schaathun.net" ""
armURI = URI { uriScheme = "https:",
           uriAuthority = Just auth,
           uriPath = "/armchar/schema",
           uriQuery = "",
           uriFragment = "#" }
rulesURI = URI { uriScheme = "https:",
           uriAuthority = Just auth,
           uriPath = "/armchar/rules",
           uriQuery = "",
           uriFragment = "#" }
armNS = makeNamespace (Just $ T.pack "arm") armURI
rulesNS = makeNamespace (Just $ T.pack "armrules") armURI

newLName s = case (QN.newLName $ T.pack s) of
   (Nothing) -> QN.emptyLName
   (Just ln) -> ln

makeRule ln s1 s2 = makeN3ClosureSimpleRule 
    rulesNS ( newLName ln ) ( fromString s1 ) ( fromString s2 )
-- fromString s = ( fromText . T.pack ) s :: Builder

csRule = makeRule "csRule" 
    "?cs <https://hg.schaathun.net/armchar/schema#isCharacter> ?c . ?c ?p ?o ."
                  "?cs ?p ?o ."

