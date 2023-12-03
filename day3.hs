import Data.Char (isDigit)
import Data.List (transpose)
testData = "467..114..\n\
        \...*......\n\
        \..35..633.\n\
        \......#...\n\
        \617*......\n\
        \.....+.58.\n\
        \..592.....\n\
        \......755.\n\
        \...$.*....\n\
        \.664.598..\n"


pad2DList :: Char ->[[Char]] -> [[Char]]
pad2DList pad ys = [pad | _ <- [-1..length $ ys !! 1]] : map (\xs -> [pad] ++ xs ++ [pad]) ys ++ [[pad | _ <- [-1..length $ ys !! 1]]] 

convert2DList :: [String] -> [String]
convert2DList ds = [[(padded !! (x+x')) !! (y+y') | x' <-[-1..1], y' <- [-1..1] ] | x <- [1..lx], y <- [1..ly]]
    where lx   = (length ds)+2
          ly   = (length (head ds)) + 2
          padded = pad2DList '.' (pad2DList '.' ds)

midVal :: String -> Char
midVal xs = xs !! 4

findNum [] = []
findNum xs = takeWhile isDigit xs : findNum (dropWhile (not.isDigit) (dropWhile isDigit xs))

batchNumbers :: [String] -> [[String]]
batchNumbers [] = []
batchNumbers xs = takeWhile (isDigit . midVal) xs : batchNumbers (dropWhile (not.isDigit.midVal) (dropWhile (isDigit.midVal) xs))

combineAreas :: [String] -> [String]
combineAreas [] = []
combineAreas [x] = splitArea x
combineAreas (x:xs) = head (splitArea x) : combineAreas (xs)


splitArea :: String -> [String]
splitArea xs = [[xs !! 0, xs !! 3, xs !! 6], [xs !! 1, xs !!4, xs !! 7], [xs !! 2, xs !! 5, xs !! 8]]

isSymbol :: Char -> Bool
isSymbol c = (not . isDigit) c && (c /= '.')

containsSymbol :: [String] -> Bool
containsSymbol = any (any isSymbol) 

getCentre :: [String] -> Int
getCentre xs = read (init (drop 1 (xs !! 1)))

day1 :: IO Int
day1 = do
    contents <- readFile "input_files/aoc23-3.txt"
    let numbers = map getCentre 
                $ filter containsSymbol 
                $ map (transpose . combineAreas) 
                $ batchNumbers 
                $ convert2DList (lines contents)
    let total = sum numbers
    return total

