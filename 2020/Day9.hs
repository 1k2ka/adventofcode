module Day9 where

getInput :: IO [Int]
getInput = fmap read . lines <$> readFile "inputs/day9"

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

isValid :: (Eq a, Num a) => [a] -> a -> Bool
isValid xs x = go (pairs xs)
    where
        go [] = False
        go ((a, b):ab) = x == a + b || go ab

findInvalid :: (Eq a, Num a) => [a] -> [a] -> a
findInvalid preamble = go (reverse preamble)
    where
        go pre (x:post)
            | isValid pre x = go (x:take 24 pre) post
            | otherwise = x

findContSum :: (Ord a, Num a) => [a] -> a -> [a]
findContSum xs x = go [] (reverse xs)
    where
        go cont (a:as)
            | length cont < 2 = go (a:cont) as
            | sum cont == x = cont
            | sum cont > x = go (init cont) (a:as)
            | sum cont < x = go (a:cont) as

main :: IO ()
main = do
    input <- getInput
    let (pre, post) = splitAt 25 input
    let part1 = findInvalid pre post
    putStrLn $ "Part 1: " ++ show part1
    let cont = findContSum input part1
    let part2 = minimum cont + maximum cont
    putStrLn $ "Part 2: " ++ show part2
