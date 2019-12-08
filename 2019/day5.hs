{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.Split
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map, (!))

-- a PC has a program counter, memory, inputs, and outputs
data PC = PC Integer (Map Integer String) [Integer] [Integer] deriving Show

getInputs :: IO [String]
getInputs = splitOn "," <$> readFile "inputs/day5"

-- Load parameter based on mode.
load :: Char -> String -> PC -> Integer
load '0' p (PC _ mem _ _) = read $ mem ! (read p)
load '1' p (PC _ _ _ _) = read p

-- Read the parameters of a three parameter instruction operation.
getParams :: (String, String, String, String) -> PC -> (Integer, Integer, Integer)
getParams ((m1:o:p:[]), p1, p2, p3) pc = (load m1 p1 pc, load '0' p2 pc, read p3)
getParams ((m2:m1:o:p:[]), p1, p2, p3) pc = (load m1 p1 pc, load m2 p2 pc, read p3)
getParams ((_:m2:m1:o:p:[]), p1, p2, p3) pc = (load m1 p1 pc, load m2 p2 pc, read p3)
getParams (_, p1, p2, p3) pc = (load '0' p1 pc, load '0' p2 pc, read p3)

-- Read the parameter of a single parameter instruction operation.
getParam :: (String, String) -> PC -> Integer
getParam ((m:o:p:[]), p1) pc = load m p1 pc
getParam (_, p) pc = load '0' p pc

-- Execute the PC once and indiciate when END instruction has been reached.
exec :: PC -> (PC, Bool)
exec p@(PC pc mem ins outs) =
  case last (mem ! pc) of
    '9' -> (p, False)
    '1' -> (opexec (+), True)
    '2' -> (opexec (*), True)
    '3' -> (PC (pc + 2) (M.insert (read $ mem ! (pc + 1)) (show $ head ins) mem) (tail ins) outs, True)
    '4' -> ((PC (pc + 2) mem ins ((getParam (mem ! pc, mem ! (pc+1)) p):outs)), True)
    _ -> undefined
  where opexec :: (Integer -> Integer -> Integer) -> PC
        opexec op =
          let (p1, p2, p3) = getParams (mem ! pc, mem ! (pc+1), mem ! (pc+2), mem ! (pc+3)) p
          in (PC (pc + 4) (M.insert p3 (show $ op p1 p2) mem) ins outs)

-- Run a PC until END.
run :: PC -> PC
run pc =
  case exec pc of
    (pc', True) -> run pc'
    (pc', False) -> pc'

count :: [a] -> [(Integer, a)]
count [] = []
count (a:as) = foldl (\acc@((i, _):_) a -> (i+1,a):acc) [(0, a)] as

main = do
  instrs <- getInputs
  let mem = (M.fromList . count) instrs
      (PC _ _ _ outs) = run (PC 0 mem [1] [])
  print $ "Part 1: " ++ (show $ head outs)

