module ArM.BasicIO where

import System.IO -- for file IO

-- | Write a list of strings into the given file
writeFile :: String     -- ^ File name
           -> [ String ] -- ^ Contents
           -> IO ()
writeFile fn c = do
     handle <- openFile fn WriteMode
     let p = hPutStrLn handle
     mapM_ p c
     hClose handle

putStrLns :: [ String ] -> IO ()
putStrLns [] = return ()
putStrLns (x:xs) = putStrLn x >> putStrLns xs
