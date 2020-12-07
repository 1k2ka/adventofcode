module Day5 where

rows = [0..127]
cols = [0..7]

toBool :: Char -> Bool
toBool 'F' = False
toBool 'L' = False
toBool 'B' = True
toBool 'R' = True

getInput :: IO [[Bool]]
getInput = do
    strs <- lines <$> readFile "inputs/day5"
    return $ fmap toBool <$> strs

partition :: [a] -> Bool -> [a]
partition xs True = drop (length xs `div` 2) xs
partition xs False = take (length xs `div` 2) xs

decode :: [Bool] -> (Integer, Integer)
decode = go rows cols
    where
        go [r] [c] _ = (r, c)
        go rs@[_] cs (b:bs) = go rs (partition cs b) bs
        go rs cs (b:bs) = go (partition rs b) cs bs

seatId :: Num a => a -> a -> a
seatId r c = r * 8 + c

main :: IO ()
main = do
    passes <- fmap decode <$> getInput
    let ids = uncurry seatId <$> passes
    let part1 = maximum ids
    putStrLn $ "Part 1: " ++ show part1
    let part2 = (uncurry seatId . head) [
            (r, c) | r <- rows, c <- cols,
            (r, c) `notElem` passes,
            seatId r c - 1 `elem` ids,
            seatId r c + 1 `elem` ids]
    putStrLn $ "Part 2: " ++ show part2
