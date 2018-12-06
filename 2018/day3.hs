module Main where

import Control.Applicative
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Data.Set (Set)
import qualified Data.Set as S

data Claim = Claim {
  getId :: Int,
  getCorner :: (Int, Int),
  getSize :: (Int, Int)
} deriving (Eq, Show)

parseSeps :: Parser ()
parseSeps = do
  many' (space <|> char '#' <|> char '@' <|> char ':' <|> char ',' <|> char 'x')
  return ()

parseClaim :: Parser Claim
parseClaim = do
  parseSeps
  id <- decimal
  parseSeps
  x <- decimal
  parseSeps
  y <- decimal
  parseSeps
  width <- decimal
  parseSeps
  height <- decimal
  endOfLine
  return $ Claim id (x, y) (width, height)

parseClaims :: Parser [Claim]
parseClaims = do
  claims <- many' parseClaim
  return claims

overlap :: Claim -> Claim -> Set (Int, Int)
overlap (Claim _ (x,y) (w,h)) (Claim _ (x', y') (w', h')) =
  S.fromList $ [(cx,cy) | cx <- [x..(x+w-1)], cy <- [y..(y+h-1)], elem cx [x'..(x'+w'-1)], elem cy [y'..(y'+h'-1)]]

overlaps :: [Claim] -> Set (Int, Int) -> Set (Int, Int)
overlaps [] ovs = ovs
overlaps (c:cs) ovs =
  let currOverlaps = foldr (\c' acc -> S.union (overlap c c') acc) S.empty cs
      newOverlaps = S.union ovs currOverlaps
  in overlaps cs newOverlaps

findNonOverlap :: [Claim] -> Int
findNonOverlap (c:cs)
  | (foldr (\c' acc -> if S.size (overlap c c') == 0 then acc else acc + 1) 0 cs) == 0 = getId c
  | otherwise = findNonOverlap (cs ++ [c])

main :: IO ()
main = do
  file <- B.readFile "inputs/day3_input"
  case parseOnly parseClaims file of
    Left _ -> putStrLn "Could not parse"
    Right claims -> do
      putStrLn $ "Part 1: " ++ (show $ length $ overlaps claims S.empty)
      putStrLn $ "Part 2: " ++ (show $ findNonOverlap claims)
