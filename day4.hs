import Data.List.Split
testData = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
            \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
            \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
            \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
            \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
            \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"


type InputLine = (Int, [Int], [Int])

parseLine :: String -> InputLine
parseLine ls = (read  (head split1), map read (head split3), map read (last split3))
            where split1 = splitOn ": " $ drop 5 ls
                  split2 = splitOn "|" (last split1)
                  split3 = map (filter (/= "") .splitOn " ") split2

countMatchs :: [Int] -> [Int] -> Int
countMatchs winners chosen = length $ filter (`elem` winners) chosen


points :: InputLine -> Int
points (_, a, b) = if winners == 0 then 0 else 2 ^ (winners-1)
      where winners = countMatchs a b

part1 = do
      contents <- readFile "input_files/aoc23-4.txt"
      let fullData = map parseLine (lines contents)
      let score = sum $ map points fullData
      return score

cardCounts :: [Int]
cardCounts = replicate 10 1

updateCardCounts:: [Int] -> Int -> Int -> [Int]
updateCardCounts xs ncards m = map (+m) (take ncards xs) ++ drop ncards xs

scoreList :: ([Int], [Int])
scoreList = unzip [(1, countMatchs a b) | (x, a, b) <- map parseLine (lines testData)]

scoreIt:: [Int] -> [Int] -> [Int]
scoreIt [] [] = []
scoreIt (count:counts) (win:wins) = count: scoreIt (updateCardCounts counts win count) wins

part2 :: IO Int
part2 = do
      contents <- readFile "input_files/aoc23-4.txt"
      let fullData = map parseLine (lines contents)
      let scoreList = unzip [(1, countMatchs a b) | (x, a, b) <- fullData]
      let cards = scoreIt (fst scoreList) (snd scoreList)
      return (sum cards)