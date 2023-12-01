import Data.List (isPrefixOf)

loadData :: String -> IO [String]
loadData f  = do
  contents <- readFile f
  let fileLines = lines contents
  return fileLines

testData = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet\n"

extractDigits :: String -> String
extractDigits [] = []
extractDigits (x:xs)
    | "one" `isPrefixOf` (x:xs)   = '1' : extractDigits xs
    | "two" `isPrefixOf` (x:xs)   = '2' : extractDigits xs
    | "three" `isPrefixOf` (x:xs) = '3' : extractDigits xs
    | "four" `isPrefixOf` (x:xs)  = '4' : extractDigits xs
    | "five" `isPrefixOf` (x:xs)  = '5' : extractDigits xs
    | "six" `isPrefixOf` (x:xs)   = '6' : extractDigits xs
    | "seven" `isPrefixOf` (x:xs) = '7' : extractDigits xs
    | "eight" `isPrefixOf` (x:xs) = '8' : extractDigits xs
    | "nine" `isPrefixOf` (x:xs)  = '9' : extractDigits xs
    | x >= '1' && x <= '9'        = x : extractDigits xs
    | otherwise                   = extractDigits xs

getFstLstNumber :: String -> Int
getFstLstNumber xs = read $ head xs : [last xs]

numList :: [String] -> [Int]
numList = map getFstLstNumber

main :: IO ()
main = do
    inputdata <-loadData "input_files/aoc23-1.txt"
    print $ sum . numList $ map extractDigits inputdata

