
module ArM.NoTrace where


trace :: String -> a -> a
trace _ y = y
ttrace :: Show a => a -> a
ttrace x = x
strace :: String -> String
strace x = x
