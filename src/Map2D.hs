module Map2D where

import qualified Data.Map.Strict as Map
import Parser
import Data.Char (digitToInt)

type Coordinates = (Integer, Integer)
type Map2D a = Map.Map Coordinates a

insert :: [a] -> Integer -> Integer -> Map2D a -> Map2D a
insert [] _ _ a = a
insert [x] a b c = Map.insert (a, b) x c
insert (x:rest) a b c =
    insert rest (a + 1) b (Map.insert (a, b) x c)

mapInsertWithIndex :: [(Integer, [a])] -> Map2D a -> Map2D a
mapInsertWithIndex [] a = a
mapInsertWithIndex [(x, y)] a = insert y 0 x a
mapInsertWithIndex ((x, y):rest) a =
    insert y 0 x (mapInsertWithIndex rest a)

getTile :: Coordinates -> Map2D a -> Maybe a
getTile = Map.lookup 

-- Parse the file and create a 2D map.
createMap2D :: FilePath -> IO (Map2D Char)
createMap2D file = do
    contents <- lineParser file
    let rows = zip [0 ..] contents -- Assign y-coordinates to each row.
    return $ mapInsertWithIndex rows Map.empty

createFromList :: [String] -> Map2D Char
createFromList [] = Map.empty
createFromList [x] = mapInsertWithIndex [(0,x)] Map.empty
createFromList x = mapInsertWithIndex (zip [0..] x) Map.empty

map2DCharToInt :: Map2D Char -> Map2D Int
map2DCharToInt  = Map.map digitToInt
