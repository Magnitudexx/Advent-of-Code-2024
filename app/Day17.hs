{-# LANGUAGE BangPatterns #-}
module Main where
import Parser
import System.Environment
import Text.Regex.Posix
import qualified Data.Map.Strict as Map
import Data.Char (digitToInt)
import Data.Bits
import Debug.Trace (trace)

matchLine :: String -> String -> [[String]]
matchLine pattern pline =  pline =~ pattern :: [[String]]

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty = foldr (\x acc -> if x == "" then [] : acc else (x : head acc) : tail acc) [[]]
executeProgram :: Integer -> Integer -> Integer -> Int -> String -> String
executeProgram regA regB regC pc program 
    | pc+1 >= length program     = ""
    | otherwise                 = case program !! pc of
        '0' -> executeProgram adv regB regC (pc+2) program
        '1' -> executeProgram regA bxl regC (pc+2) program
        '2' -> trace (show bst) $ executeProgram regA bst regC (pc+2) program
        '3' -> executeProgram regA regB regC newPc program
        '4' -> executeProgram regA bxc regC (pc+2) program
        '5' -> out ++ executeProgram regA regB regC (pc+2) program
        '6' -> executeProgram regA adv regC (pc+2) program
        '7' -> executeProgram regA regB adv (pc+2) program
        _   -> ""
    where
        newPc = if regA /= 0 then lit else pc+2
        adv = regA `div` 2^co
        bst = co `mod` 8
        bxc = regB `xor` regC 
        bxl = regB `xor` toInteger lit
        out = (show bst ++ ",")
        lit = digitToInt (program !! (pc+1)) 
        co = case lit of
            4 -> regA
            5 -> regB
            6 -> regC
            7 -> error "Not valid program"
            a   -> toInteger a

part1 :: IO ()
part1 = do
    listOfLines <- lineParser "inputs/day17.txt"
    let
        withoutBlank = splitOnEmpty listOfLines
        registerList = head withoutBlank
        programList = concat (concat (tail withoutBlank))
        p1 = "Register [A-Z]: ([0-9]+)"
        p2 = "Program: ([0-9,]+)"
        registerValues = concatMap tail (concatMap (matchLine p1) registerList)
        regA = read (head registerValues) :: Integer
        regB = read (registerValues !! 1) :: Integer
        regC = read (registerValues !! 2) :: Integer 
        program = filter (/=',') (concat (tail (concat (matchLine p2 programList))))
        str = reverse (drop 1 (reverse (executeProgram regA regB regC 0 program)))
    print str
    print "1"

part2 :: IO ()
part2 = do
    print "1"
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    --["part2"]   -> part2
    _           -> putStr "I do not know"
