module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B

data Tree = Tree [Tree] [Int] deriving (Eq, Show)

parseMeta :: Parser Int
parseMeta = do
  i <- decimal
  space
  return i

parseTree :: Parser Tree
parseTree = do
  noChild <- decimal
  space
  noMeta <- decimal
  space
  children <- count noChild parseTree
  meta <- count noMeta parseMeta
  return $ Tree children meta

sumMeta :: Tree -> Int
sumMeta (Tree trees meta) =
  (sum meta) + (foldr ((+) . sumMeta) 0 trees)

nodeVal :: Tree -> Int
nodeVal (Tree [] meta) = sum meta
nodeVal (Tree trees []) = 0
nodeVal (Tree trees (m:ms))
  | m == 0 || (m-1) >= length trees = nodeVal (Tree trees ms)
  | otherwise = nodeVal (trees !! (m-1)) + nodeVal (Tree trees ms)

main = do
  file <- B.readFile "inputs/day8_input"
  case parseOnly parseTree file of
    Left _ -> putStrLn "Failed to parse file"
    Right tree -> do
      putStrLn $ "Part 1: " ++ (show $ sumMeta tree)
      putStrLn $ "Part 2: " ++ (show $ nodeVal tree)
