{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}

module Main where

import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as I

data GameState = GameState {
  circle :: S.Seq Int,
  scores :: I.IntMap Int,
  currentMarble :: Int,
  nextMarble :: Int,
  currPlayer :: Int
} deriving (Eq, Show)

players :: Int
players = 452

initGame :: GameState
initGame = GameState (S.fromList [0, 2, 1]) I.empty 1 3 3

nextState :: Int -> GameState -> GameState
nextState lm gs@(GameState circle scores ci nm cp)
  | nm > lm = gs
  | mod nm 23 == 0 =
      let rmi = (ci-7) `mod` (S.length circle)
          rm = S.index circle rmi
          scores' = I.insertWith (+) cp (nm+rm) scores
          circle' = S.deleteAt rmi circle
          cp' = (cp+1) `mod` players
      in GameState circle' scores' rmi (nm+1) cp'
  | otherwise =
      let cl = S.length circle
          ii = if ci + 2 == cl then cl else (ci + 2) `mod` cl
          circle' = S.insertAt ii nm circle
          cp' = (cp+1) `mod` players
      in GameState circle' scores ii (nm+1) cp'

playGame :: Int -> GameState -> Int
playGame lm gs =
  let ns = nextState lm gs
  in if | ns == gs -> maximum $ map snd $ I.toList $ scores gs
        | otherwise -> playGame lm ns

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ (show $ playGame 71250 initGame)
  putStrLn $ "Part 2: " ++ (show $ playGame (71250*100) initGame)
