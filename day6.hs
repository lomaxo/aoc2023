module Main where

validTimes ::  Float -> Float -> [Int]
validTimes totalTime distance = filter (\c -> (fromIntegral c * (totalTime - fromIntegral c))>distance ) [1..round totalTime]

countallvalid :: [(Float, Float)] -> Int
countallvalid input = product $ map (length . uncurry validTimes) input

main:: IO ()
main = do
    print (countallvalid [(61677571, 430103613071150)])
    return ()