module Day6 where

import Data.List ( nub )
import Data.List.Split ( splitOn )

getInput :: IO [String]
getInput = splitOn "\n\n" <$> readFile "inputs/day6"

part1 :: [String] -> Int
part1 = foldr ((+) . length . nub . concat . lines) 0

part2 :: [String] -> Int
part2 [] = 0
part2 (x:xs) = part2 xs + length [q | q <- questions, and $ elem q <$> answers]
    where 
        answers = lines x
        questions = (nub . concat) answers

main = do
    groups <- getInput
    putStrLn $ "Part 1: " ++ show (part1 groups)
    putStrLn $ "Part 2: " ++ show (part2 groups)
