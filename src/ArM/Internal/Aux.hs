module ArM.Internal.Aux where

import Data.List (sort)

-- | Sort the list and remove duplicates.
uniqueSort :: (Ord a,Eq a) => [a] -> [a]
uniqueSort = f . sort
    where f [] = []
          f (x:[]) = x:[]
          f (x:y:ys) | x == y = f (y:ys)
          f (x:y:ys) | x /= y = x:f (y:ys)
