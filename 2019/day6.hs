module Main where

import Data.List (union, intersect, (\\))
import Data.Maybe (isJust)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

data OrbitMap = OM String [OrbitMap] deriving Show

getInput :: IO [(String, String)]
getInput = fmap ((\(x,y) -> (x, tail y)) . splitAt 3) <$> lines <$> readFile "inputs/day6"

constructMap :: [(String, String)] -> OrbitMap
constructMap = buildMap "COM" . go M.empty
  where
    go :: Map String [String] -> [(String, String)] -> Map String [String]
    go m ((p,o):xs) =
      go (M.alter appOrIns p m `M.union` M.singleton o []) xs
      where
        appOrIns :: Maybe [String] -> Maybe [String]
        appOrIns (Just os) = Just (o:os)
        appOrIns Nothing = Just [o]
    go m [] = m

    buildMap :: String -> Map String [String] -> OrbitMap
    buildMap k m =
      case M.lookup k m of
        Just ss -> OM k (map (`buildMap` m) ss)
        Nothing -> OM k []

countOrbits :: OrbitMap -> Integer
countOrbits = go 0
  where
    go i (OM _ os) = foldr ((+) . go (i+1)) i os

path :: String -> OrbitMap -> Maybe [String]
path s (OM s' os)
  | s == s' = Just []
  | otherwise = case [p | p <- map (path s) os, isJust p] of
                  (Just p:[]) -> Just (s':p)
                  [] -> Nothing

main :: IO ()
main = do
  om <- constructMap <$> getInput
  print $ "Part 1: " ++ show (countOrbits om)
  case traverse (`path` om) ["YOU", "SAN"] of
    Just (p1:p2:[]) -> print $ "Part 2: " ++ show (length $ p1 `union` p2 \\ p1 `intersect` p2)
    Nothing -> print "Failed to find common path."
