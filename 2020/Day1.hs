module Day1 where

import Data.Maybe ( isJust )

getInput :: IO [Integer]
getInput = do
    integerStrs <- lines <$> readFile "inputs/day1"
    return $ read <$> integerStrs

findYear :: [Integer] -> Integer -> Maybe (Integer, Integer)
findYear [] _ = Nothing
findYear (x:xs) i =
    case i + x of
        2020 -> Just (i, x)
        _ -> findYear xs i

findThree :: [Integer] -> [Integer] -> Integer -> Maybe Integer
findThree [] _ _ = Nothing
findThree _ [] _ = Nothing
findThree (x:xs) ys i =
    case findYear ys (x + i) of
        Just (_, y) -> Just (x * i * y)
        Nothing -> findThree xs ys i

day1 :: IO ()
day1 = do
    nums <- getInput
    let Just (x, y) = (head . filter isJust) $ findYear nums <$> nums
    putStrLn $ "Part 1: " ++ show (x * y)
    let Just part2 = (head . filter isJust) $ findThree nums nums <$> nums
    putStrLn $ "Part 2: " ++ show part2
