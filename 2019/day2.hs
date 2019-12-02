module Main where

import Debug.Trace

split :: Char -> String -> [String]
split c str =
  let (s, rest) = break (==c) str
  in case rest of
    (',':ss) -> s : split c ss
    _ -> [s]

getInput :: IO [Int]
getInput = (fmap . fmap) read $ split ',' <$> readFile "inputs/day2"

exec :: Int -> [Int] -> [Int]
exec pointer positions =
  case positions !! pointer of
    99 -> positions
    1 -> exec (pointer + 4) $ left ++ (x + y) : right
    2 -> exec (pointer + 4) $ left ++ (x * y) : right
    where x = positions !! (positions !! (pointer + 1))
          y = positions !! (positions !! (pointer + 2))
          save = positions !! (pointer + 3)
          (left, (_:right)) = splitAt save positions

setMem :: Int -> Int -> [Int] -> [Int]
setMem noun verb (zero:_:_:pos) = (zero:noun:verb:pos)

gridSearch :: [Int] -> (Int, Int)
gridSearch positions = head $ dropWhile ((/=19690720) . result) grid
  where grid = [(x, y) | x <- [0..99], y <- [0..99]]
        result :: (Int, Int) -> Int
        result (noun, verb) = exec 0 (setMem noun verb positions) !! 0

main = do
  input <- getInput
  print $ "Part 1: " ++ (show $ exec 0 (setMem 12 2 input) !! 0)
  let (noun, verb) = gridSearch input
  print $ "Part 2: " ++ (show $ 100 * noun + verb)

