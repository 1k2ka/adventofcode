module Day2 where

import Data.Bits ( xor )
import Data.List.Split ( splitOn )

data Policy = Policy {
    letter :: Char,
    low :: Integer,
    high :: Integer
} deriving Show

policy :: String -> (Policy, String)
policy s = (Policy letter (read low) (read high), password)
    where
        (range:letterColon:password:_) = splitOn " " s
        (low:high:_) = splitOn "-" range
        letter = head letterColon

getInput :: IO [(Policy, String)]
getInput = do
    fileStrs <- lines <$> readFile "inputs/day2"
    return $ policy <$> fileStrs

count :: Eq a => a -> [a] -> Integer
count x ys = sum [1 | y <- ys, y == x]

part1 :: [(Policy, String)] -> Integer
part1 [] = 0
part1 ((pol, pass):xs) =
    if cnt >= low pol && cnt <= high pol
        then 1 + part1 xs
        else part1 xs
    where
        cnt = count (letter pol) pass

part2 :: [(Policy, String)] -> Integer
part2 [] = 0
part2 ((pol, pass):xs) =
    if (pass !! i == c) `xor` (pass !! j == c)
        then 1 + part2 xs
        else part2 xs
    where
        i = fromInteger $ low pol - 1
        j = fromInteger $ high pol - 1
        c = letter pol

main :: IO ()
main = do
    input <- getInput
    putStrLn $ "Part 1: " ++ (show . part1) input
    putStrLn $ "Part 2: " ++ (show . part2) input
