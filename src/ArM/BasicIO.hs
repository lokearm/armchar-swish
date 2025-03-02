module ArM.BasicIO where

import System.IO -- for file IO

-- | Write a list of strings into the given file
writeLns :: String     -- ^ File name
           -> [ String ] -- ^ Contents
           -> IO ()
writeLns fn c = do
     handle <- openFile fn WriteMode
     let p = hPutStrLn handle
     mapM_ p c
     hClose handle

writeMaybeFile :: Maybe String     -- ^ File name
           -> [ String ]           -- ^ Contents
           -> IO ()
writeMaybeFile Nothing _ = return ()
writeMaybeFile (Just x) y =  writeLns x y

putStrLns :: [ String ] -> IO ()
putStrLns [] = return ()
putStrLns (x:xs) = putStrLn x >> putStrLns xs
