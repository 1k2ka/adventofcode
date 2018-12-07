{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.Char

data Dep = Dep {
  _idDep :: Char,
  _dep :: Char
} deriving (Eq, Show)

data Step = Step {
  _id :: Char,
  _deps :: [Char]
} deriving (Eq, Show)

data Worker = Worker {
  _wId :: Int,
  _work :: [(Char, Int)]
} deriving (Eq, Show)

type Graph = HashMap Char [Char]

instance Ord Step where
  compare (Step sid _) (Step sid' _) = compare sid sid'

instance Ord Worker where
  compare (Worker _ []) (Worker _ _) = LT
  compare (Worker _ _) (Worker _ []) = GT
  compare (Worker _ work) (Worker _ work') =
    cmpWork (maximumBy cmpWork work) (maximumBy cmpWork work')

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
getTime c i = ord(c) - 4 + i

cmpWork :: (Char, Int) -> (Char, Int) -> Ordering
cmpWork (c, t) (c', t') = compare (getTime c t) (getTime c' t')

mergeWorkers :: [Worker] -> [Worker] -> [Worker]
mergeWorkers [] w = w
mergeWorkers (w:ws) workers =
  let wids = map _wId workers
  in if | elem (_wId w) wids -> mergeWorkers ws (w:(filter ((/=_wId w) . _wId) workers))
        | otherwise -> mergeWorkers ws (w:ws)

occupyWorkers :: [Worker] -> [Char] -> Int -> [Worker]
occupyWorkers workers [] _ = workers
occupyWorkers [] _ _ = []
occupyWorkers (w:ws) (c:cs) t =
  (Worker (_wId w) ((c, t):(_work w))):(occupyWorkers ws cs t)

completedTasks :: Int -> Worker -> [Char]
completedTasks t w = map fst $ filter ((<=t) . (uncurry getTime)) $ _work w

multiPlan :: [Worker] -> Graph -> Int -> [Worker]
multiPlan workers graph time =
  let availableWorkers = sortBy (\w w' -> compare (_wId w) (_wId w')) $ filter (and . (map ((<= time) . (uncurry getTime))) . _work) workers
      completedTasks' = nub $ concat $ map (completedTasks time) workers
      runningTasks = concat $ map ((map fst) . _work) workers
      availableTasks = H.foldrWithKey (\k v acc -> if (and $ map ((flip elem) completedTasks') v) then k:acc else acc) [] graph
      todoTasks = sort $ (availableTasks \\ completedTasks') \\ runningTasks
  in if | length completedTasks' == H.size graph -> workers
        | otherwise ->
            let occupiedWorkers = occupyWorkers availableWorkers todoTasks time
                newWorkers = mergeWorkers occupiedWorkers workers
            in multiPlan newWorkers graph (time + 1)

findLatest :: [Worker] -> Int
findLatest = (uncurry getTime) . (maximumBy cmpWork) . _work . maximum

main :: IO ()
main = do
  file <- B.readFile "inputs/day7_input"
  case parseOnly parseSteps file of
    Left _ -> putStrLn "Failed to parse"
    Right deps -> do
      let steps = sort $ depsToSteps deps
          plan' = plan steps
          graph = stepsToGraph steps
          workers = [(Worker x []) | x <- [1..5]]
      putStrLn $ "Part 1: " ++ (show plan')
      putStrLn $ "Part 2: " ++ (show $ findLatest $ multiPlan workers graph 0)
