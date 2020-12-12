module Day10 where

import Prelude hiding ((<>), (**))
import Data.List ( sort )
import Numeric.LinearAlgebra
    ( atIndex, toZ, matrix, (<>), Matrix, R )

getInput :: IO [Int]
getInput = fmap read . lines <$> readFile "inputs/day10"

pairs :: [a] -> [(a, a)]
pairs xs = concat $ (\x -> ((,) x) <$> xs) <$> xs

(**) :: Matrix R -> Int -> Matrix R
m ** 1 = m
m ** n = m <> (m ** (n - 1))

adjMat :: (Ord a, Num a) => [a] -> Matrix R
adjMat xs = matrix n (f <$> coords)
    where
        n = length xs
        ns = [0..n-1]
        coords = pairs ns
        f (n1, n2)
            | n2 > n1 && j - i <= 3 = 1
            | n1 == (n - 1) && n2 == (n - 1) = 1
            | otherwise = 0
            where
                i = xs !! n1
                j = xs !! n2

main :: IO ()
main = do
    input <- sort <$> getInput
    let jolts = (0:input) ++ [maximum input + 3]
    let diffs = uncurry (-) <$> zip (tail jolts) jolts
    let filterCnt x = length . filter (==x)
    let part1 = product ([filterCnt] <*> [1, 3] <*> [diffs])
    putStrLn $ "Part 1: " ++ show part1
    let mat = adjMat jolts
    let n = length jolts
    let part2 = toZ (mat ** n) `atIndex` (0, n - 1)
    putStrLn $ "Part 2: " ++ show part2
