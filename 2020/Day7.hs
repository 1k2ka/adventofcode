module Day7 where

import Data.List.Split ( splitOn )
import Data.Char ( isDigit )
import Data.Map.Lazy ( Map, (!) )
import qualified Data.Map.Lazy as Map

parseContent :: [String] -> (Integer, String)
parseContent ("":cnt:mod:col:_) = (read cnt, mod ++ col)
parseContent (cnt:mod:col:_) = (read cnt, mod ++ col)

parseBag :: String -> (String, [(Integer, String)])
parseBag xs = (color, parseContent <$> filter ((>1) . length) bags)
    where
        words = splitOn " " xs
        color = (concat . take 2) words
        bags = splitOn " " <$> (splitOn "," . dropWhile (not . isDigit)) xs

type Input = Map String [(Integer, String)]

getInput :: IO Input
getInput = do
    rules <- lines <$> readFile "inputs/day7"
    return $ Map.fromList $ parseBag <$> rules

canFit :: Input -> String -> Int
canFit m color = length $ filter fitsIn bags
    where
        bags = Map.keys m
        fitsIn :: String -> Bool
        fitsIn s =
            let bagsIn = snd <$> (m ! s)
            in color `elem` bagsIn || or (fitsIn <$> bagsIn)

subBags :: Input -> String -> Integer
subBags m color = go (m ! color)
    where
        go [] = 0
        go ((cnt,col):xs) = cnt * (1 + subBags m col) + go xs

main :: IO ()
main = do
    input <- getInput
    let part1 = canFit input "shinygold"
    putStrLn $ "Part 1: " ++ show part1
    let part2 = subBags input "shinygold"
    putStrLn $ "Part 2: " ++ show part2
