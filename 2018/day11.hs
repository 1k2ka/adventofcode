module Main where

import qualified Data.HashMap.Lazy as H
import Data.List

gsn :: Int
gsn = 3999

powerLevel :: (Int, Int) -> Int
powerLevel (x, y) =
  let rackId = x + 10
      pl = (rackId * y + gsn) * rackId `div` 100
  in pl `mod` 10 - 5

square :: Int -> (Int, Int) -> [(Int, Int)]
square s (x', y') = [(x,y) | x <- [x'..(x'+s-1)], y <- [y'..(y'+s-1)]]

squarePower :: [(Int, Int)] -> Int
squarePower = foldr ((+) . powerLevel) 0

saVal :: (Int, Int) -> Int
saVal (x, y) =
  sum $ map powerLevel [(x', y') | x' <- [1..x], y' <- [1..y]]

satVal :: H.HashMap (Int, Int) Int -> (Int, Int) -> Int -> Int
satVal map' (x,y) w =
  let a = map' H.! (x-1, y-1)
      b = map' H.! (x+w-1,y-1)
      c = map' H.! (x-1,y+w-1)
      d = map' H.! (x+w-1,y+w-1)
  in d + a - b - c

largestSqure :: H.HashMap (Int, Int) Int -> (Int, Int) -> (Int, Int)
largestSqure map' (x, y) =
  let maxX = 300-x
      maxY = 300-y
      w    = min maxX maxY
      w' = maximumBy (\s s' -> satVal map' (x,y) s `compare` satVal map' (x,y) s') [1..w]
  in (w', satVal map' (x,y) w')

main :: IO ()
main = do
  let squares = map (square 3) [(x,y) | x <- [0..297], y <- [0..297]]
      bestSquare = head $ maximumBy (\sq sq' -> squarePower sq `compare` squarePower sq') squares
      sat = foldr (\c map' -> H.insert c (saVal c) map') H.empty [(x,y) | x <- [0..300], y <- [0..300]]
      bestTot = maximumBy (\x y -> (snd . snd) x `compare` (snd . snd) y) $ map (\c -> (c, largestSqure sat c)) [(x,y) | x <- [1..299], y <- [1..299]]
  putStrLn $ "Part 1: " ++ (show $ bestSquare)
  putStrLn $  "Part 2: " ++ (show $ fst $ bestTot) ++ " size=" ++ (show $ fst $ snd bestTot)
