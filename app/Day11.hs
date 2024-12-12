module Main where

import Parser
import System.Environment
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- Tree structure to hold strings and their level information
data Tree = Node 
  { value :: String            -- Value of the string
  , children :: [Tree]         -- Children nodes
  , levelMap :: Map.Map Int [String] -- Map of levels to their values
  } deriving (Show, Eq)

-- Create a new node
createNode :: String -> [Tree] -> Tree
createNode val childs = Node val childs (buildLevelMap 0 childs)

-- Helper to build the level map
buildLevelMap :: Int -> [Tree] -> Map.Map Int [String]
buildLevelMap level childs =
  let currentLevel = Map.singleton level (map value childs)
      childMaps = map (\child -> buildLevelMap (level + 1) (children child)) childs
  in foldr (Map.unionWith (++)) currentLevel childMaps

-- Apply rules to all strings in the tree
applyRules :: [String -> String] -> Tree -> Tree
applyRules rules (Node val childs _) =
  let newValue = foldr (\rule acc -> rule acc) val rules
      newChildren = map (applyRules rules) childs
  in Node newValue newChildren (buildLevelMap 0 newChildren)

-- Example rule: Add a prefix to strings
addPrefixRule :: String -> String -> String
addPrefixRule prefix str = prefix ++ str

-- Example rule: Convert string to uppercase
uppercaseRule :: String -> String
uppercaseRule = map toUpper
part1 :: IO ()
part1 = do
    initialStones <- wordParser "./inputs/test11.txt"
    print "1"
part2 :: IO ()
part2 = do
    initialStones <- wordParser "./inputs/day11.txt"
    print "2"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    ["part2"]   -> part2
    _           -> putStr "I do not know"
