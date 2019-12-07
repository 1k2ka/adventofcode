module Main where

import Data.List

isSorted :: Ord a => [a] -> Bool
isSorted (x:y:xs) = x <= y && isSorted (y:xs)
isSorted _ = True

adjacent :: Eq a => (Int -> Bool) -> [a] -> Bool
adjacent f = any (f . length) . group

isLegit :: String -> Bool
isLegit s = isSorted s && adjacent (>=2) s

isLegit' :: String -> Bool
isLegit' s = isSorted s && adjacent (==2) s

possible  = length [x | x <- [145852..616942], isLegit  (show x)]
possible' = length [x | x <- [145852..616942], isLegit' (show x)]

main = do
  print $ "Part 1: " ++ show possible
  print $ "Part 2: " ++ show possible'

