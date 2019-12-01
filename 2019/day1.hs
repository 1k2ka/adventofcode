module Main where

getInput :: IO [Integer]
getInput = map read . lines <$> readFile "inputs/day1"

handleMass :: Integer -> Integer
handleMass mass = floor (fromIntegral mass / 3) - 2

handleMass' :: Integer -> Integer
handleMass' mass =
  if fuel <= 0 then 0 else fuel + handleMass' fuel
  where fuel = handleMass mass

main = do
  masss <- getInput
  let tot = (sum . map handleMass) masss
  let tot' = (sum . map handleMass') masss
  print $ "Part 1: " ++ show ((sum . map handleMass) masss)
  print $ "Part 2: " ++ show ((sum . map handleMass') masss)

