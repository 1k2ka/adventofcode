module Main where

countChar :: Char -> String -> Int
countChar c xs = length $ filter (==c) xs

remChar :: Char -> String -> String
remChar c xs = filter (/=c) xs

dupsAndTrips :: String -> (Int, Int)
dupsAndTrips [] = (0, 0)
dupsAndTrips (x:xs) =
  let cnt = countChar x (x:xs)
      newStr = remChar x xs
      (dups, trips) = dupsAndTrips newStr
  in case cnt of
       3 -> (dups, 1)
       2 -> (1, trips)
       _ -> (dups, trips)

sumTup :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTup (x, y) (x', y') = (x + x', y + y')

isSimilar :: String -> String -> Bool
isSimilar xs ys = go 0 xs ys
    where
      go 1 [] [] = True
      go diffs (x:xs) (y:ys)
        | diffs > 1 = False
        | x == y = go diffs xs ys
        | otherwise = go (diffs + 1) xs ys
      go _ _ _ = False

findSimilar :: [String] -> String
findSimilar (x:xs) =
  let similars = filter (isSimilar x) xs
  in
    case similars of
      [] -> findSimilar xs
      (y:_) -> zipWith (\x y -> if x == y then x else '_') x y

main :: IO ()
main = do
  file <- words <$> (readFile "inputs/day2_input")
  let (d, t) = foldr1 sumTup $ map dupsAndTrips file
  putStrLn $ "Part 1: " ++ (show $ d * t)
  putStrLn $ "Part 2: " ++ (show $ findSimilar file)
