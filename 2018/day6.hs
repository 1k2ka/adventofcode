{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import qualified Data.HashMap.Lazy as H
import Data.List

type Coord = (Int, Int)
type Map = H.HashMap Coord (Maybe Int)

parseCoord :: Parser Coord
parseCoord = do
  x <- decimal
  string ", "
  y <- decimal
  endOfLine
  return (x, y)

parseCoords :: Parser [Coord]
parseCoords = many' parseCoord >>= return

manhattan :: Coord -> Coord -> Int
manhattan (x, y) (x', y') = abs(x-x') + abs(y-y')

maxXY :: [Coord] -> (Int, Int)
maxXY = foldr (\(x, y) (x', y') -> (max x x', max y y')) (0,0)

labelCoord :: [Coord] -> Coord -> Map -> Map
labelCoord coords coord map' =
  let minCoord = minimumBy (\c c' -> compare (manhattan c coord) (manhattan c' coord)) coords
      minIndex = elemIndex minCoord coords
  in H.insert coord minIndex map'

createMap :: (Int, Int) -> [Coord] -> Map
createMap (maxX, maxY) coords =
  let allCoords = [(x,y) | x <- [0..maxX], y <- [0..maxY]]
  in foldr (labelCoord coords) H.empty allCoords

diqualified :: (Int, Int) -> Map -> [Maybe Int]
diqualified (maxX, maxY) map' =
  let borderCoords = [(x, y)| x <- [0..maxX], y <- [0, maxY]] ++ [(x, y)| y <- [1..(maxY-1)], x <- [0, maxX]]
  in nub $ foldr (\coord acc -> (map' H.! coord):acc) [] borderCoords

rankMap :: Map -> [Maybe Int] -> Int
rankMap map' disqs =
  let
    countList :: [(Maybe Int, Int)]
    countList = H.toList $ foldr (\v acc -> H.insertWith (+) v 1 acc) H.empty map'
  in snd $ head $ reverse $ sortOn snd $ filter (not . ((flip elem) disqs) . fst) countList

withinRegion :: [Coord] -> Coord -> Int
withinRegion coords coord
  | foldr (\x acc -> acc + (manhattan coord x)) 0 coords < 10000 = 1
  | otherwise = 0

findCloseRegion :: (Int, Int) -> [Coord] -> Int
findCloseRegion (maxX, maxY) coords =
  let allCoords = [(x,y) | x <- [0..maxX], y <- [0..maxY]]
  in sum $ map (withinRegion coords) allCoords

main :: IO ()
main = do
  file <- B.readFile "inputs/day6_input"
  case parseOnly parseCoords file of
    Left _ -> putStrLn "Failed to parse file"
    Right coords -> do
      let bounds = maxXY coords
          map' = createMap bounds coords
          disqs = diqualified bounds map'
          largestSize = rankMap map' disqs
      putStrLn $ "Part 1: " ++ (show largestSize)
      putStrLn $ "Part 2: " ++ (show $ findCloseRegion bounds coords)
