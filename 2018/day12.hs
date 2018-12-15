{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.ByteString                  as B
import qualified Data.HashMap.Lazy                as H

type Rules = H.HashMap String Char

pot = char '#' <|> char '.'

parseState :: Parser String
parseState = do
  string "initial state: "
  state <- many' pot
  endOfLine
  return state

parseRule :: Parser (String, Char)
parseRule = do
  pattern <- manyTill pot space
  string "=> "
  result <- pot
  endOfLine
  return (pattern, result)

parseInput :: Parser (String, Rules)
parseInput = do
  state <- parseState
  endOfLine
  rules <- many' parseRule
  return (state, H.fromList rules)

applyRule :: Rules -> String -> Char
applyRule rules s =
  case H.lookup s rules of
    Just c  -> c
    Nothing -> error "Incorrect input"

applyRules :: Rules -> String -> String
applyRules rules st@(s:s':ss) = s:s':(go rules st)
  where
    go rules state
      | length state >= 5 =
          let seg = take 5 state
          in (applyRule rules seg):(go rules (tail state))
      | otherwise = ".."

goGen :: Rules -> String -> Int -> String
goGen rules state i
  | i == 0 = state
  | otherwise =
      let nextGen = applyRules rules ("..." ++ state ++ "...")
      in goGen rules nextGen (i-1)

genScore :: Rules -> String -> Int -> Int
genScore r s i =
  let fs = goGen r s i
      fs' = reverse $ foldr (\x acc -> (1 + (fst $ head acc), x):acc) [(-i*3-1, '.')] $ reverse fs
  in sum $ map fst $ filter ((=='#') . snd) fs'

main :: IO ()
main = do
  file <- B.readFile "inputs/day12_input"
  case parseOnly parseInput file of
    Left _ -> putStrLn "Failed to parse"
    Right (state, rules) -> do
      putStrLn $ "Part 1: " ++ (show $ genScore rules state 20)
      -- Converges at 151
      let convGen = 151
          convScore = genScore rules state convGen
          convDiff = (genScore rules state (convGen + 1)) - convScore
          convSum = convScore + (50000000000 - convGen) * convDiff
      putStrLn $ "Part 2: " ++ (show convSum)
