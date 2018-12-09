{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Data.Sequence as S
import qualified Data.HashMap.Lazy as H
import Debug.Trace

data GameState = GameState {
  circle :: S.Seq Int, -- Make infinite?
  scores :: H.HashMap Int Int,
  currentMarble :: Int,
  nextMarble :: Int,
  currPlayer :: Int
} deriving (Eq, Show)

players :: Int
players = 452

lastMarble :: Int
lastMarble = 71250

initGame :: GameState
initGame = GameState (S.fromList [0, 2, 1]) H.empty 2 3 3

currMarbleIndex :: GameState -> Int
currMarbleIndex (GameState circle _ cm _ _) =
  case S.findIndexL (==cm) circle of
    Just i -> i
    Nothing -> error "Invalid gamestate"

insertIndex :: GameState -> Int
insertIndex gs@(GameState circle _ _ _ _) =
  let cmi = currMarbleIndex gs
      cl  = S.length circle
  in if | cmi + 2 == cl -> cl
        | otherwise -> (cmi + 2) `mod` cl

nextState :: GameState -> GameState
nextState gs@(GameState circle scores cm nm cp)
  | cm >= lastMarble = gs
  | mod nm 23 == 0 =
      let cmi = currMarbleIndex gs
          rmi = ((cmi - 7) `mod` S.length circle)
          rm  = circle `S.index` rmi
          scores' = H.insertWith (+) cp (nm+rm) scores
          circle' = S.deleteAt rmi circle
          cm' = circle' `S.index` rmi
          cp' = if (cp+1) `mod` (players+1) == 0 then 1 else cp+1
      in GameState circle' scores' cm' (nm+1) cp'
  | otherwise =
      let cmi = currMarbleIndex gs
          ii = insertIndex gs
          circle' = S.insertAt ii nm circle
          cp' = if (cp+1) `mod` (players+1) == 0 then 1 else cp+1
      in GameState circle' scores nm (nm+1) cp'

playGame :: GameState -> Int
playGame gs
  | (nextState gs) == gs = maximum $ map snd $ H.toList $ scores gs
  | otherwise = playGame (nextState gs)

main = putStrLn $ show $ playGame initGame
