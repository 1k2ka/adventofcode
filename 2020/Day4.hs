module Day4 where

import Data.List.Split ( splitOn )
import Data.List ( isPrefixOf )
import Data.Char ( isLetter, isDigit, isAlphaNum )

getInput :: IO [[String]]
getInput = do
    input <- splitOn "\n\n" <$> readFile "inputs/day4"
    return $ words <$> input

requiredKeys :: [String]
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isPassport :: [String] -> Bool
isPassport = null . foldr removeKey requiredKeys
    where removeKey x = filter (not . (`isPrefixOf` x))

data FieldType = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid
data Field = Field FieldType String

makeField :: String -> Field
makeField x =
    case fieldType of
        "byr" -> Field Byr s
        "iyr" -> Field Iyr s
        "eyr" -> Field Eyr s
        "hgt" -> Field Hgt s
        "hcl" -> Field Hcl s
        "ecl" -> Field Ecl s
        "pid" -> Field Pid s
        "cid" -> Field Cid s
    where
        fieldType = take 3 x
        s = drop 4 x

validateField :: Field -> Bool
validateField (Field Byr s) = let i = read s in length s == 4 && 1920 <= i && i <= 2002
validateField (Field Iyr s) = let i = read s in length s == 4 && 2010 <= i && i <= 2020
validateField (Field Eyr s) = let i = read s in length s == 4 && 2020 <= i && i <= 2030
validateField (Field Hgt s) =
    case unit of
        "cm" -> 150 <= len && len <= 193
        "in" -> 59 <= len && len <= 76
        _ -> False
    where
        unit = filter isLetter s
        len = read $ filter isDigit s
validateField (Field Hcl s) = head s == '#' && ((==6) . length . filter isAlphaNum) s
validateField (Field Ecl s) = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validateField (Field Pid s) = length s == 9 && all isDigit s
validateField (Field Cid _) = True

validatePassport :: [String] -> Bool
validatePassport = and . (<*>) [isPassport, all valid] . (:[])
    where valid = validateField . makeField

main :: IO ()
main = do
    input <- getInput
    let part1 = length $ filter isPassport input
    putStrLn $ "Part 1: " ++ show part1
    let part2 = length $ filter validatePassport input
    putStrLn $ "Part 2: " ++ show part2
