import Data.List.Split
type ColourSet = (Int, Int, Int)
type Game = (Int, [ColourSet])

loadData :: String -> IO [String]
loadData f  = do
  contents <- readFile f
  let fileLines = lines contents
  return fileLines

testLine = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

testData = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
            \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
            \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
            \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
            \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"

parseData :: String -> [Game]
parseData xs = map parseLine (lines xs)

parseLine :: String -> Game
parseLine xs = (read a, parseList bs) where a: [bs] = splitOn ": " (drop 5 xs)

parseList :: String -> [ColourSet]
parseList xs = map (getColourTuple . splitOn ", ") $ splitOn "; " xs

--parseList xs = map getColourTuple (map (splitOn ", ") $ splitOn "; " xs)

parseTest xs = map (splitOn ", ") $ splitOn "; " xs

getColourTuple :: [String] -> ColourSet
getColourTuple xs = (sum [getNofThing x "red" | x <- xs], sum [getNofThing x "green" | x <- xs], sum [getNofThing x "blue" | x <- xs])

getNofThing :: String -> String -> Int
getNofThing xs thing = 
    let a: bs = splitOn " " xs in
    if head bs == thing then read a else 0


isValidGame :: ColourSet -> Bool
isValidGame (r, g, b) = r <= 12 && g <= 13 && b <= 14

sumValid :: [Game] -> Int
sumValid games = sum [id | (id, attempts) <- games, all isValidGame attempts]

part1 :: IO Int
part1 = do
    gamedata <- readFile "./input_files/aoc23-2.txt"
    return $ sumValid (parseData gamedata)

minNeeded :: [ColourSet] -> [Int]
minNeeded cs = [maximum rs, maximum gs, maximum bs]
    where (rs, gs, bs) = unzip3 cs

part2 :: IO Int
part2 = do    
    gamedata <- readFile "./input_files/aoc23-2.txt"
    let games = parseData gamedata
    let mins = [product (minNeeded sets) | (id, sets) <- games]
    return (sum mins)
