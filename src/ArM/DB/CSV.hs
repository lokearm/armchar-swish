-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.DB.CSV
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  The ArMCSV class supports parsing a type from CSV files.
--
--
-----------------------------------------------------------------------------
module ArM.DB.CSV where

import qualified Data.Map as M
import qualified Data.CSV as CSV
import Text.ParserCombinators.Parsec

class ArMCSV t where
    -- | Parse the cells of one line from the CSV file into a SpellRecord object.
    fromCSVline :: [String] -> t
    defaultObject :: t
    getID :: t -> String

    -- | Create a `Data.Map.Map` of SpellRecord objects.  
    -- The input is the output from `Data.CSV.csvFile`
    getDB :: [[String]] -> M.Map String t
    getDB = M.fromList . map ( \ x -> (getID x,x) ) . map fromCSVline

    -- | Read spells from CSV.  Return Maybe SpellDB.
    readDB :: String -- ^ Filename
              -> IO (Maybe (M.Map String t))
    readDB fn = parseFromFile CSV.csvFile fn >>= return . Just . getDB . g
      where g (Left _) = [[]]
            g (Right x) = x
