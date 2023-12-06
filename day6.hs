module Main where

validTimes totalTime distance = filter (\c -> (fromIntegral c * (totalTime - fromIntegral c))>distance ) [1..round totalTime]

countallvalid input = product $ map (length . uncurry validTimes) input
main:: IO ()
main = do
    print (countallvalid [(61677571, 430103613071150)])
    return ()