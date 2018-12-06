module Main where

import Control.Applicative
import qualified Data.IntSet as IS
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8

type FreqChange = Int

parseFreq :: Parser FreqChange
parseFreq = do
  sign <- (char '-' <|> char '+')
  val <- many' digit
  endOfLine
  case sign of
    '-' -> return $ (*) (-1) $ read val
    '+' -> return $ read val

parseFreqs :: Parser [FreqChange]
parseFreqs = do
  vals <- many' parseFreq
  return vals

findFirstDup :: IS.IntSet -> [Int] -> Maybe Int
findFirstDup _ [] = Nothing
findFirstDup seen (x:xs) =
  case IS.member x seen of
    True -> Just x
    False -> findFirstDup (IS.insert x seen) xs

main :: IO ()
main = do
  file <- B.readFile "inputs/day1_input"
  case parseOnly parseFreqs file of
    Left err -> putStrLn "Could not parse file"
    Right vals -> do
      let part1 = last $ scanl (+) 0 vals
          part2 = findFirstDup IS.empty $ scanl (+) 0 (cycle vals)
      putStrLn $ "Part 1: " ++ (show part1)
      putStrLn $ "Part 2: " ++ (show part2)
