{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Data.List
import qualified Data.HashMap.Lazy as HM

data Action =
    BeginShift Int
  | FallAsleep
  | WakeUp
  deriving (Eq, Show)

data Record = Record {
  _date :: Int,
  _time :: (Int, Int),
  _action :: Action
} deriving Eq

data Guard = Guard {
  _id :: Int,
  _ms :: [Int]
} deriving (Eq, Show)

instance Ord Record where
  compare (Record date (h,m) _) (Record date' (h', m') _)
    | date == date' && h == h' = compare m m'
    | date == date' = compare h h'
    | otherwise = compare date date'

instance Ord Guard where
  compare (Guard _ m) (Guard _ m') = compare (length m) (length m')

parseAction :: Parser Action
parseAction = do
  firstChar <- anyChar
  case firstChar of
    'f' -> return FallAsleep
    'w' -> return WakeUp
    _   -> do
      string "uard #"
      id <- decimal
      return $ BeginShift id

parseRecord :: Parser Record
parseRecord = do
  char '['
  year <- many' digit
  char '-'
  month <- many' digit
  char '-'
  day <- many' digit
  space
  hour <- decimal
  char ':'
  minute <- decimal
  char ']'
  space
  action <- parseAction
  manyTill anyChar endOfLine
  return $ Record (read $ year ++ month ++ day) (hour, minute) action

parseRecords :: Parser [Record]
parseRecords = do
  records <- many' parseRecord
  return records

hmToGuards :: HM.HashMap Int [Int] -> [Guard]
hmToGuards hm =
  HM.foldrWithKey (\k v acc -> (Guard k v):acc) [] hm

getGuards :: [Record] -> [Guard]
getGuards records = go records HM.empty 0 0
  where
    go [] hm _ _ = hmToGuards hm
    go (r:rs) hm gid ptime =
      case _action r of
        BeginShift newGid -> go rs hm newGid 0
        FallAsleep -> go rs hm gid (snd $ _time r)
        WakeUp -> go rs (HM.insertWith (++) gid [ptime..((snd $ _time r) - 1)] hm) gid 0

findMinute :: Guard -> (Int, Int, Int)
findMinute (Guard gid mins) =
  let minmap = foldr (\m acc -> HM.insertWith (+) m 1 acc) HM.empty mins
  in HM.foldrWithKey (\k v (m, count, _) -> if v >= count then (k, v, gid) else (m, count, gid)) (0,0,gid) minmap

cmpMin :: (Int, Int, Int) -> (Int, Int, Int) -> Ordering
cmpMin (m, c, gid) (m', c', gid') = compare c c'

main :: IO ()
main = do
  file <- B.readFile "inputs/day4_input"
  case parseOnly parseRecords file of
    Left _ -> putStrLn "Failed to parse"
    Right recs -> do
      let records = sort recs
          guards = getGuards records
          guard = maximum guards
          (m, _, _) = findMinute guard
          guardMins = map findMinute guards
          (m', _, gid) = maximumBy cmpMin guardMins
      putStrLn $ concat [ "Part 1: Guard #"
                        , (show $ _id guard)
                        , " Minute: "
                        , (show m)
                        , " Result: "
                        , (show $ m * (_id guard))
                        ]
      putStrLn $ concat [ "Part 2: Guard #"
                        , (show $ gid)
                        , " Minute: "
                        , (show m')
                        , " Result: "
                        , (show $ m' * gid)
                        ]
