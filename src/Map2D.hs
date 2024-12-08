module Map2D  where

import qualified Data.Map.Strict as Map
import Parser

type Coordinates = (Integer, Integer)
type Map2D  = Map.Map Coordinates Char

insert :: [Char] -> Integer -> Integer -> Map2D  -> Map2D 
insert [] _ _ a = a
insert [x] a b c = Map.insert (a,b) x c
insert (x:rest) a b c =
    insert rest (a+1) b (Map.insert (a,b) x c)

mapInsertWithIndex :: [(Integer, [Char])] -> Map2D  -> Map2D 
mapInsertWithIndex [] a = a
mapInsertWithIndex [(x,y)] a = insert y 0 x a
mapInsertWithIndex ((x,y):rest) a = 
    insert y 0 x (mapInsertWithIndex rest a)

getTile :: Coordinates -> Map2D -> Char
getTile a b = case Map.lookup a b of 
    Just value  -> value
    Nothing     ->'!'

-- Parse the file and create a 2D map.
createMap2D :: FilePath -> IO Map2D 
createMap2D file = do
    contents <- lineParser file
    let rows = zip [0 ..] contents -- Assign y-coordinates to each row.
    return $ mapInsertWithIndex rows Map.empty
