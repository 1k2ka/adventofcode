{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Data.HashMap.Strict as H
import Safe
import Data.Complex
import Data.List (delete, find, sortBy)

type CartMap = H.HashMap (Double, Double) Char
type Cart = (Complex Double, Complex Double, Bool)

trd :: (a, b, c) -> c
trd (_, _, c) = c

parseMap :: [String] -> [((Double, Double), Char)]
parseMap x = filter ((/=' ') . snd) $ go x 0
  where
    go :: [String] -> Double -> [((Double, Double), Char)]
    go [] _ = []
    go (s:ss) y =
      let r = foldr (\c acc -> case acc of
                                  [] -> ((y, fromIntegral $ length s - 1), c):acc
                                  _  -> ((y, (snd . fst . head) acc - 1), c):acc) [] s
      in r ++ (go ss (y+1))

cartFilter :: Char -> Bool
cartFilter c = elem c "^v<>"

r :: Complex Double
r = 1 :+ 0

-- i represents clockwise turn and down
cw :: Complex Double
cw = 0 :+ 1

-- -i represents counter-clockwise turn and up
ccw :: Complex Double
ccw = 0 :+ (-1)

getCarts :: [((Double, Double), Char)] -> [((Double, Double), Cart)]
getCarts xs =
  let carts = filter (cartFilter . snd) xs
      m' = map (\(yx, c) -> case c of
                                '^' -> (yx, (ccw, ccw, True))
                                '>' -> (yx, (r, ccw, True))
                                'v' -> (yx, (cw, ccw, True))
                                '<' -> (yx, (-r, ccw, True))) carts
  in m'

moveCarts :: CartMap -> [((Double, Double), Cart)] -> [((Double, Double), Cart)]
moveCarts _ [] = []
moveCarts cm (c@((y,x), (dir, nt, st)):cs) =
  if st then
    let ndir = if cm H.! (y,x) == '+' then dir * nt else dir
        infront = headMay $ filter (\((y',x'), _) -> (x :+ y) + ndir == (x' :+ y')) cs
    in case infront of
      Just i@(yx, _) -> moveCarts cm (((yx, (r, r, False))):(delete i cs))
      Nothing ->
        let r = cm H.! (y,x)
        in if | r == '+' ->
                let dir' = (dir * nt)
                    nt' = if nt == cw then ccw else nt * cw
                    (x' :+ y') = (x :+ y) + dir'
                in ((y', x'), (dir', nt', True)):(moveCarts cm cs)
              -- clockwise
              |    (r == '\\' && (dir == 1 || dir == -1))
                || (r == '/' && (dir == cw || dir == ccw)) ->
                  let dir' = dir * cw
                      (x' :+ y') = (x :+ y) + dir'
                  in ((y', x'), (dir', nt, True)):(moveCarts cm cs)
              -- counter-clockwise
              |    (r == '\\' && (dir == cw || dir == ccw))
                || (r == '/' && (dir == 1 || dir == -1)) ->
                  let dir' = dir * ccw
                      (x' :+ y') = (x :+ y) + dir'
                  in ((y', x'), (dir', nt, True)):(moveCarts cm cs)
              -- straight
              | otherwise ->
                  let (x' :+ y') = (x :+ y) + dir
                  in ((y', x'), (dir, nt, True)):(moveCarts cm cs)
  else c:(moveCarts cm cs)

checkCollisions :: [((Double, Double), Cart)] -> [((Double, Double), Cart)]
checkCollisions [] = []
checkCollisions (c@(yx, _):cs) =
  case find ((==yx) . fst) cs of
    Just c' -> checkCollisions ((yx, (r, r, False)):(delete c' cs))
    Nothing -> c:(checkCollisions cs)

tick :: CartMap -> [((Double, Double), Cart)] -> [((Double, Double), Cart)]
tick cm = checkCollisions . (moveCarts cm)

tick' :: Int -> CartMap -> [((Double, Double), Cart)] -> [((Double, Double), Cart)]
tick' i cm cs = if i <= 0 then cs else tick' (i-1) cm (tick cm cs)

hasCollisions :: [((Double, Double), Cart)] -> Bool
hasCollisions cs = any (not . trd . snd) cs

removeCollisions :: [((Double, Double), Cart)] -> [((Double, Double), Cart)]
removeCollisions = filter (trd . snd)

part1 :: CartMap -> [((Double, Double), Cart)] -> [((Double, Double), Cart)]
part1 cm cs =
  let ns = sortBy cmpCart $ tick cm cs
  in if hasCollisions ns then filter (not . trd . snd) ns else part1 cm ns

part2 :: CartMap -> [((Double, Double), Cart)] -> [((Double, Double), Cart)]
part2 cm cs =
  let ns = sortBy cmpCart $ removeCollisions $ tick cm cs
  in if length ns == 1 then ns else part2 cm ns

cmpCart :: ((Double, Double), Cart) -> ((Double, Double), Cart) -> Ordering
cmpCart (yx, _) (yx', _) = yx `compare` yx'

main = do
  f <- readFile "inputs/day13_input"
  let m' = parseMap $ lines f
      cm = H.fromList $ map (\(yx, c) -> case c of
                                          '<' -> (yx, '-')
                                          '>' -> (yx, '-')
                                          'v' -> (yx, '|')
                                          '^' -> (yx, '|')
                                          _   -> (yx, c)) m'
      cs = sortBy cmpCart $ getCarts m'
      (y, x) = (fst . head) $ part1 cm cs
      (y', x') = (fst . head) $ part2 cm cs
  putStrLn $ "Part 1: " ++ (show x) ++ "," ++ (show y)
  putStrLn $ "Part 2: " ++ (show x') ++ "," ++ (show y')
