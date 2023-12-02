module AocTools where

loadData :: String -> IO [String]
loadData f  = do
  contents <- readFile f
  let fileLines = lines contents
  return fileLines

