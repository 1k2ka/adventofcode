{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.Split
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map, (!))

type Memory = Map Integer String
type Input = [Integer]
type Output = [Integer]
type IP = Integer
data PC = PC IP Memory Input Output deriving Show

getInputs :: IO [String]
getInputs = splitOn "," <$> readFile "inputs/day5"

-- Load parameter based on mode.
load :: Char -> String -> Memory -> Integer
load '0' p mem = read $ mem ! (read p)
load '1' p _   = read p
load _ _ _     = undefined

-- Read the parameters of a three parameter instruction operation.
getParam3 :: (String, String, String, String) -> Memory -> (Integer, Integer, Integer)
getParam3 ((m1:_:_:[]),      p1, p2, p3) mem = (load m1  p1 mem, load '0' p2 mem, read p3)
getParam3 ((m2:m1:_:_:[]),   p1, p2, p3) mem = (load m1  p1 mem, load m2  p2 mem, read p3)
getParam3 ((_:m2:m1:_:_:[]), p1, p2, p3) mem = (load m1  p1 mem, load m2  p2 mem, read p3)
getParam3 (_,                p1, p2, p3) mem = (load '0' p1 mem, load '0' p2 mem, read p3)

-- Read the parameters of a two parameter instruction operation.
getParam2 :: (String, String, String) -> Memory -> (Integer, Integer)
getParam2 ((m1:_:_:[]),      p1, p2) mem = (load m1  p1 mem, load '0' p2 mem)
getParam2 ((m2:m1:_:_:[]),   p1, p2) mem = (load m1  p1 mem, load m2  p2 mem)
getParam2 ((_:m2:m1:_:_:[]), p1, p2) mem = (load m1  p1 mem, load m2  p2 mem)
getParam2 (_,                p1, p2) mem = (load '0' p1 mem, load '0' p2 mem)

-- Read the parameter of a single parameter instruction operation.
getParam :: (String, String) -> Memory -> Integer
getParam ((m:_:_:[]), p1) mem = load m   p1 mem
getParam (_, p)           mem = load '0' p  mem

-- Execute the PC once and indiciate when END instruction has been reached.
exec :: PC -> Either Integer PC
exec (PC ip mem ins outs)
  | length instr > 1 && drop (length instr - 3) instr == "99" = Left (head outs)
  | otherwise =
    Right $ case last instr of
      '1' -> opexec (+)
      '2' -> opexec (*)
      '3' -> PC (ip + 2) (M.insert (read $ mem ! (ip + 1)) (show $ head ins) mem) (tail ins) outs
      '4' -> PC (ip + 2) mem ins ((getParam (mem ! ip, mem ! (ip+1)) mem):outs)
      '5' -> ifelse (/=0)
      '6' -> ifelse (==0)
      '7' -> cmp (<)
      '8' -> cmp (==)
      _ -> undefined
  where
    instr :: String
    instr = mem ! ip

    at :: Integer -> String
    at = (!) mem

    p2s :: (String, String, String)
    p2s = (at ip, at $ ip + 1, at $ ip + 2)

    p3s :: (String, String, String, String)
    p3s = (at ip, at $ ip + 1, at $ ip + 2, at $ ip + 3)

    ifelse :: (Integer -> Bool) -> PC
    ifelse f
      | f p1      = PC p2 mem ins outs
      | otherwise = PC (ip + 3) mem ins outs
      where (p1, p2) = getParam2 p2s mem

    cmp :: (Integer -> Integer -> Bool) -> PC
    cmp f = PC (ip + 4) (M.insert p3 (if f p1 p2 then "1" else "0") mem) ins outs
      where (p1, p2, p3) = getParam3 p3s mem

    opexec :: (Integer -> Integer -> Integer) -> PC
    opexec op = (PC (ip + 4) (M.insert p3 (show $ op p1 p2) mem) ins outs)
      where (p1, p2, p3) = getParam3 p3s mem

-- Run a PC until END.
run :: PC -> Integer
run pc =
  case exec pc of
    Right pc' -> run pc'
    Left out -> out

enumerate :: [a] -> [(Integer, a)]
enumerate xs = go xs 0
  where
    go :: [a] -> Integer -> [(Integer, a)]
    go []     _ = []
    go (a:as) i = (i, a) : (go as (i+1))

main :: IO ()
main = do
  instrs <- getInputs
  let mem = (M.fromList . enumerate) instrs
      o1 = run (PC 0 mem [1] [])
      o2 = run (PC 0 mem [5] [])
  print $ "Part 1: " ++ (show $ o1)
  print $ "Part 2: " ++ (show $ o2)
