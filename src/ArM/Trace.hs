module ArM.Trace where

import qualified Debug.Trace as T

trace :: String -> a -> a
trace = T.trace
ttrace :: Show a => a -> a
ttrace x = trace (show x) x
strace :: String -> String
strace x = trace  x x
