module Day3 where

getInput :: IO [String]
getInput = lines <$> readFile "inputs/day3"

infCols :: [[a]] -> [[a]]
infCols = map (concat . repeat)

play :: Num t => (Int, Int) -> t -> [[Char]] -> (Int, Int) -> t
play (row, col) score g (row', col')
    | row >= length g = score
    | otherwise =
        case (g !! row) !! col of
            '#' -> play newPos (score + 1) g (row', col')
            _ -> play newPos score g (row', col')
            where newPos = (row + row', col + col')

main :: IO ()
main = do
    input <- getInput
    let infGrid = infCols input
    let play' = play (0, 0) 0 infGrid
    let part1 = play' (1, 3)
    putStrLn $ "Part 1: " ++ show part1
    let part2 = (product . fmap play') [(1, 3), (1, 1), (1, 5), (1, 7), (2, 1)]
    putStrLn $ "Part 2: " ++ show part2
