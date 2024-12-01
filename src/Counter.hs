module Counter where

import qualified Data.Map.Strict as Map 

type Counter k = Map.Map k Int

insert :: Ord k => k -> Counter k -> Counter k
insert x  = Map.insertWith  (+) x 1

fromList :: Ord k => [k] -> Counter k  
fromList = foldr insert Map.empty
