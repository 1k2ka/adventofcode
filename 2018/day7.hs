{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.Char
import Debug.Trace

data Dep = Dep {
  _idDep :: Char,
  _dep :: Char
} deriving (Eq, Show)

data Step = Step {
  _id :: Char,
  _deps :: [Char]
} deriving (Eq, Show)

data Worker = Worker {
  _work :: [(Char, Int)]
} deriving (Eq, Show)

type Graph = HashMap Char [Char]

instance Ord Step where
  compare (Step sid _) (Step sid' _) = compare sid sid'

parseStep :: Parser Dep
parseStep = do
  string "Step "
  id <- anyChar
  string " must be finished before step "
  dep <- anyChar
  manyTill anyChar endOfLine
  return $ Dep id dep

parseSteps :: Parser [Dep]
parseSteps = many' parseStep

depsToSteps :: [Dep] -> [Step]
depsToSteps deps =
  let ids = nub . concat . map (\(Dep did dep) -> [did,dep]) $ deps
      initmap = foldr (\c map' -> H.insert c "" map') H.empty ids
      depMap = foldr (\(Dep did dep) map' -> H.insertWith (++) (dep) ([did]) map') initmap deps
  in map (uncurry Step) $ H.toList depMap

canPlan :: [Char] -> Step -> Bool
canPlan planned (Step _ deps) = and $ map ((flip elem) planned) deps

plan :: [Step] -> [Char]
plan steps = go steps []
  where
    go :: [Step] -> [Char] -> [Char]
    go [] planned = reverse $ planned
    go steps planned =
      case findIndex (canPlan planned) steps of
        Nothing -> []
        Just i ->
          let step = steps !! i
          in go (delete step steps) ((_id step):planned)

stepsToGraph :: [Step] -> Graph
stepsToGraph [] = H.empty
stepsToGraph (s:ss) = H.singleton (_id s) (_deps s) `H.union` (stepsToGraph ss)

getTime :: Char -> Int -> Int
getTime '_' i = i
getTime c i = i + (ord c - 4)

mapWorkerToTasks :: [Char] -> Int -> Worker -> (Worker, [Char])
mapWorkerToTasks [] _ w = (w, [])
mapWorkerToTasks (c:cs) time (Worker work) = (Worker ((c, time):work), cs)

findNextTime :: [Worker] -> Int
findNextTime workers =
  minimum $ map ((uncurry getTime) . safeMaxBy . _work) workers
  where
    safeMaxBy :: [(Char, Int)] -> (Char, Int)
    safeMaxBy [] = ('_', 0)
    safeMaxBy work = maximumBy cmpWork work
    cmpWork (c, t) (c', t') = compare (getTime c t) (getTime c t)

-- multiPlan :: [Worker] -> Graph -> Int -> [Worker]
multiPlan workers graph time =
  let availableWorkers = filter (and . (map ((< time) . (uncurry getTime))) . _work) workers
      completedTasks = concat $ map ((map fst) . _work) availableWorkers
      availableTasks = H.foldrWithKey (\k v acc -> if (and $ map ((flip elem) completedTasks) v) then k:acc else acc) [] graph
      todoTasks = availableTasks \\ completedTasks
  in if | length completedTasks == H.size graph -> workers
        | length todoTasks == 0 || length availableWorkers == 0 -> multiPlan workers graph (findNextTime workers)
        | otherwise ->
            let occupiedWorkers = map fst $ init $ foldr (\w x@((_, tasks):_) -> (mapWorkerToTasks tasks time w):x) [(Worker [], todoTasks)] availableWorkers
                nextTime = findNextTime occupiedWorkers
            in trace (show nextTime) $ multiPlan occupiedWorkers graph nextTime

main :: IO ()
main = do
  file <- B.readFile "inputs/day7_input"
  case parseOnly parseSteps file of
    Left _ -> putStrLn "Failed to parse"
    Right deps -> do
      let steps = sort $ depsToSteps deps
          plan' = plan steps
          graph = stepsToGraph steps
          workers = [Worker [], Worker [], Worker [], Worker [], Worker []]
      putStrLn $ "Part 1: " ++ (show plan')
      putStrLn $ show (multiPlan workers graph 0)
