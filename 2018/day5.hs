module Main where

import Data.Char
import Data.List

willReact :: Char -> Char -> Bool
willReact c c'
  | (isLower c) && (isUpper c') && (toUpper c) == c' = True
  | (isUpper c) && (isLower c') && (toUpper c') == c = True
  | otherwise = False

reactPolymer :: String -> String
reactPolymer polymer =
  let polymer' = foldr go [] polymer
  in if polymer' == polymer then polymer else reactPolymer polymer'
  where
    go x [] = [x]
    go x (y:ys)
      | willReact x y = ys
      | otherwise = x:y:ys

main :: IO ()
main = do
  polymer <- readFile "inputs/day5_input"
  let reactedPolymer = reactPolymer polymer
      bestReaction = minimum $ map (length . reactPolymer . (\c -> filter ((/=(toLower c)) . toLower) reactedPolymer)) ['a'..'z']
  putStrLn $ "Part 1: " ++ (show $ length reactedPolymer)
  putStrLn $ "Part 2: " ++ (show bestReaction)
