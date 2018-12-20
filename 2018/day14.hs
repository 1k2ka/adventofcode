{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Strict #-}

module Main where

import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import Data.List
import Data.Foldable

data State = State {
  elf1 :: Int,
  elf2 :: Int,
  recipes :: Seq Int
} deriving Show

combine :: Int -> Seq Int
combine ((`divMod` 10)->(x,y))
  | x == 0 = S.fromList [y]
  | otherwise = S.fromList [x,y]

initS :: State
initS = State 0 1 (S.fromList [3, 7])

nextS :: State -> State
nextS (State e1 e2 rs) =
  let 
    r1 = rs `S.index` e1
    r2 = rs `S.index` e2
    rs' = rs >< (combine (r1 + r2))
    l = S.length rs'
  in State ((e1 + 1 + r1) `mod` l) ((e2 + 1 + r2) `mod` l) rs'

score :: Int -> State -> String
score i (State _ _ rs) =
  concat $ map show $ toList $ (S.take 10) . (S.drop i) $ rs

part1 :: Int -> State -> String
part1 l s =
  let s' = nextS s
      l' = S.length $ recipes $ s'
  in if l' - l >= 10 then score l s' else part1 l s'

findSeq :: Int -> [Int] -> [Int] -> Maybe Int
findSeq _ _ [] = Nothing
findSeq i xs y@(_:ys)
  | xs `isPrefixOf` y = Just i
  | otherwise = findSeq (i+1) xs ys

part2 :: [Int] -> State -> Int
part2 is s = go 0 is s
  where
    go i is s
      | i > 10000000 =
          let s' = nextS s
              sl = toList $ recipes s'
          in case findSeq 0 is sl of
            Just i -> i
            Nothing -> go 0 is s'
      | otherwise = go (i+1) is (nextS s)

main = do
  putStrLn $ "Part 1: " ++ (part1 554401 initS)
  putStrLn $ "Part 2: " ++ (show $ part2 [5,5,4,4,0,1] initS)
