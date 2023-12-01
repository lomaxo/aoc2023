import Data.List.Split.Internals
import Data.List
import qualified Data.Ord
-- import Data.ByteString (split)

readnumbers :: IO Integer
readnumbers = do
  contents <- readFile "input_files/numbers.txt"
  let fileLines = lines contents
  -- Type annotation is needed to let GHC know we want to convert
  -- to a list of Integer and not (say) a list of Floats
  let integers = map read fileLines :: [Integer]
  return $ sum integers

readnumber2 = do
    integers <- map read . lines <$> readFile "input_files/numbers.txt" :: IO [Integer]
    return $ sum integers

readpairs = do
    d <- lines <$> readFile "input_files/pairs.txt"
    -- let n = map words d
    let p = [map read ps  | ps <- map words d] :: [[Integer]]
    return p

calories = do
    groups <- splitOn "\n\n" <$> readFile "input_files/test2.txt"
    let splitgroups = map (splitOn "\n") groups
    let cals = sum $ take 3 (sortBy (Data.Ord.comparing Data.Ord.Down) ([sum $ map read x | x <- splitgroups, x /= []]) :: [Integer])
    return cals
-- splitBy :: Char -> String -> [String]