import Data.Map (Map, fromList, lookup)
import Data.List.Split ( splitOn )
import GHC.Parser.Lexer (xset)
import Debug.Trace
test_data :: String
test_data = "RL\n\
    \AAA = (BBB, CCC)\n\
    \BBB = (DDD, EEE)\n\
    \CCC = (ZZZ, GGG)\n\
    \DDD = (DDD, DDD)\n\
    \EEE = (EEE, EEE)\n\
    \GGG = (GGG, GGG)\n\
    \ZZZ = (ZZZ, ZZZ)"

-- directions = head $ lines test_data

mapStr = drop 1 $  lines test_data

-- getKeyFromData = take 3

getLRPairFromData ds = (head xs, take 3 $ drop 1 $ last xs) 
    where xs = splitOn "," $ drop 7 ds

getKeyDataPair ds = (take 3 ds, getLRPairFromData ds)

m = fromList $ map getKeyDataPair mapStr

getNextL :: Map String (String, String) -> String -> Char -> Maybe String
getNextL m k d = do
    p <- Data.Map.lookup k m
    if d == 'L' then Just $ fst p else Just $ snd p

rotList (x:xs) = xs ++ [x]

traverse' :: (Map String (String, String),Int, Maybe String) -> String -> (Int, Maybe String)
traverse' (_, r, Nothing) _        = (r, Nothing)
traverse' (_, r, Just "ZZZ") _   = (r, Just "ZZZ")
traverse' (m, r, Just ys) ds      = trace (show r ++ " " ++ ys ++ show ds) traverse' (m, r+1, getNextL m ys (head ds)) (rotList ds)


-- part1 :: IO ()
part1 = do
    contents <- readFile "input_files/aoc23-8.txt"
    let m = fromList $ map getKeyDataPair (drop 1 $  lines contents)
    let n = (traverse' (m, 0, Just "AAA") (head $ lines contents))
    return n
      