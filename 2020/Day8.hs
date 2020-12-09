module Day8 where

import Data.Maybe ( isJust )

data Instruction =
      Acc Int
    | Jmp Int
    | Nop Int

data Computer = Computer {
    _accum :: Int,
    _pc :: Int,
    _history :: [Int],
    _program :: [Instruction]
}

readSigned :: String -> Int
readSigned ('+':i) = read i
readSigned ('-':i) = -(read i)

readInstr :: String -> Instruction
readInstr str =
    case op of
        "acc" -> Acc i
        "jmp" -> Jmp i
        "nop" -> Nop i
    where
        op = take 3 str
        i = (readSigned . drop 4) str

getInput :: IO [Instruction]
getInput = fmap readInstr . lines <$> readFile "inputs/day8"

runComputer :: Computer -> Computer
runComputer (Computer acc pc history program) =
    case program !! pc of
        Acc i -> Computer (acc + i) (pc + 1) (pc:history) program
        Jmp i -> Computer acc (pc + i) (pc:history) program
        Nop _ -> Computer acc (pc + 1) (pc:history) program

runToInfiniteLoop :: Computer -> Computer
runToInfiniteLoop c
    | _pc c `elem` _history c = c
    | otherwise = runToInfiniteLoop (runComputer c)

terminates :: Computer -> Maybe Computer
terminates c
    | _pc c `elem` _history c = Nothing
    | _pc c >= (length . _program) c = Just c
    | otherwise = terminates (runComputer c)

replaceJmp :: [Instruction] -> [Instruction] -> [[Instruction]]
replaceJmp _ [] = []
replaceJmp h (i:is) =
    case i of
        Jmp c -> (h ++ (Nop c:is)):replaceJmp (h ++ [i]) is
        _ -> replaceJmp (h ++ [i]) is

main :: IO ()
main = do
    instr <- getInput
    let computer = Computer 0 0 []
    let part1 = (_accum . runToInfiniteLoop) (computer instr)
    putStrLn $ "Part 1: " ++ show part1
    let jmps = replaceJmp [] instr
    let part2 = (head . filter isJust) (terminates <$> (computer <$> jmps))
    case part2 of
        Just c -> putStrLn $ "Part 2: " ++ show (_accum c)
