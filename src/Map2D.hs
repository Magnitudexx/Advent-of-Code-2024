module Map2D where

import qualified Data.Map.Strict as Map
import Parser

type Coordinates = (Integer, Integer)
type Map2D k = Map.Map Coordinates k

insert :: [k] -> Integer -> Integer -> Map2D k -> Map2D k
insert [] _ _ a = a
insert [x] a b c = Map.insert (a,b) x c
insert (x:rest) a b c =
    insert rest (a+1) b (Map.insert (a,b) x c)

mapInsertWithIndex :: [(Integer, [k])] -> Map2D k -> Map2D k
mapInsertWithIndex [] a = a
mapInsertWithIndex [(x,y)] a = insert y 0 x a
mapInsertWithIndex ((x,y):rest) a = 
    insert y 0 x (mapInsertWithIndex rest a)

-- Parse the file and create a 2D map.
createMap2D :: FilePath -> IO (Map2D Char)
createMap2D file = do
    contents <- lineParser file
    let rows = zip [0 ..] contents -- Assign y-coordinates to each row.
    return $ mapInsertWithIndex rows Map.empty
