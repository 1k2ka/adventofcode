module Main where

import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as S

getInput :: IO ([String], [String])
getInput = do
  file <- readFile "inputs/day3"
  let ls = lines file
  return (splitOn "," (head ls), splitOn "," (ls !! 1))

constructMap :: (Int, Int) -> [String] -> Set (Int, Int)
constructMap _ [] = S.empty
constructMap (cx, cy) (p:ps) =
  case p of
    ('U':a) -> S.fromList [(cx, y) | y <- [cy+1..cy+read a]] `S.union` constructMap (cx, cy + read a) ps
    ('D':a) -> S.fromList [(cx, y) | y <- [cy-read a..cy-1]] `S.union` constructMap (cx, cy - read a) ps
    ('L':a) -> S.fromList [(x, cy) | x <- [cx-read a..cx-1]] `S.union` constructMap (cx - read a, cy) ps
    ('R':a) -> S.fromList [(x, cy) | x <- [cx+1..cx+read a]] `S.union` constructMap (cx + read a, cy) ps

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

cmp :: (Int, Int) -> (Int, Int) -> Ordering
cmp p1 p2 = manhattan (0,0) p1 `compare` manhattan (0,0) p2

main = do
  (ps1, ps2) <- getInput
  let m1 = constructMap (0,0) ps1
      m2 = constructMap (0,0) ps2
      i = m1 `S.intersection` m2
      closest = minimumBy cmp (S.toList i)
  print $ "Part 1: " ++ show (manhattan (0,0) closest)

