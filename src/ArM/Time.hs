module ArM.Time where
import Numeric
import System.CPUTime

showf f = showFFloat (Just 3) t1 "" 
            where t1 = fromIntegral ( f `div` 10^9 ) * 10**(-3)
printTime :: IO ()
printTime = do
     t1 <- getCPUTime
     print $ "Total CPU Time: " ++ showf t1 ++ "s"
