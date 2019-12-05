module Main where

import Data.List.Split
import Data.HashMap (Map)
import qualified Data.HashMap as HM

getInput :: IO ([String], [String])
getInput = do
  file <- readFile "inputs/day3"
  let ls = lines file
  return (splitOn "," (head ls), splitOn "," (ls !! 1))

constructMap :: Int -> (Int, Int) -> [String] -> Map (Int, Int) Int
constructMap _ _ [] = HM.empty
constructMap tot (x, y) ((d:a):ps) =
  HM.fromList (map (go d) walk) `HM.union` constructMap (tot + amount) next ps
  where
      amount = read a
      walk = [1..amount]
      next = (fst . go d) amount

      go :: Char -> Int -> ((Int, Int), Int)
      go 'U' i = ((x, y + i), tot + i)
      go 'D' i = ((x, y - i), tot + i)
      go 'L' i = ((x - i, y), tot + i)
      go 'R' i = ((x + i, y), tot + i)

part1 :: Map (Int, Int) Int -> Map (Int, Int) Int -> Int
part1 m1 m2 = minimum (map (manhattan (0,0) . fst) $ HM.toList intersection)
  where
    intersection = m1 `HM.intersection` m2

    manhattan :: (Int, Int) -> (Int, Int) -> Int
    manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

part2 :: Map (Int, Int) Int -> Map (Int, Int) Int -> Int
part2 m1 m2 = minimum (map snd $ HM.toList intersection)
  where
    intersection = HM.intersectionWith (+) m1 m2

main = do
  (ps1, ps2) <- getInput
  let m1 = constructMap 0 (0, 0) ps1
      m2 = constructMap 0 (0, 0) ps2
  print $ "Part 1: " ++ show (part1 m1 m2)
  print $ "Part 2: " ++ show (part2 m1 m2)

