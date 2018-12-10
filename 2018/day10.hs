{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Data.List

data Point = Point (Int, Int) (Int, Int) deriving (Eq, Show)

showPoints :: [(Int, Int)] -> String
showPoints points =
  let minY = snd $ minimumBy (\x y -> snd x `compare` snd y) points
      minX = fst $ minimumBy (\x y -> fst x `compare` fst y) points
      points' = map (\(x,y) -> (x-minX, y-minY)) points
      maxY = snd $ maximumBy (\x y -> snd x `compare` snd y) points'
      maxX = fst $ maximumBy (\x y -> fst x `compare` fst y) points'
  in concat $ map (\y -> (map (\x -> if elem (x,y) points' then '#' else '.') [0..maxX]) ++ "\n") [0..maxY]

parsePoint :: Parser Point
parsePoint = do
  string "position=<"
  many' space
  x <- signed decimal
  string ", "
  many' space
  y <- signed decimal
  string "> velocity=<"
  many' space
  xv <- signed decimal
  string ", "
  many' space
  yv <- signed decimal
  manyTill anyChar endOfLine
  return $ Point (x, y) (xv, yv)

advance :: Int -> Point -> (Int, Int)
advance i (Point (x,y) (xv,yv)) = (x+i*xv,y+i*yv)

align :: (Int, Int) -> [(Int, Int)] -> Int
align _ [] = 0
align (_, y) ((_, y'):ps)
  | y == y' = 0
  | otherwise = 1

metric :: [Point] -> Int -> Int
metric points i =
  let ap = map (advance i) points
  in foldr (\p s -> s + (align p ap)) 0 ap

main :: IO ()
main = do
  file <- B.readFile "inputs/day10_input"
  case parseOnly (many' parsePoint) file of
    Left _ -> putStrLn "Failed to parse file"
    Right points ->
      let ap = map (metric points) [0..20000]
          min = minimum ap
          mini = findIndex (==min) ap
      in case mini of
        Just i -> do
          putStrLn $ "Part 1:\n" ++ (showPoints $ map (advance i) points)
          putStrLn $ "Part 2: " ++ (show i)
        Nothing -> putStrLn ""
