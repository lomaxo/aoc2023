
chargeTime :: Float -> Float -> Float
chargeTime totalTime distance = (totalTime + sqrt (totalTime^2 - (4 * distance))) / 2

validTimes :: Float -> Float -> [Int]
validTimes totalTime distance = filter (\c -> (fromIntegral c * (totalTime - fromIntegral c))>distance ) [1..round totalTime]

-- distance c t = c * (t - c)

-- isValid d t c = (c * (t - c))>=d

-- countallvalid :: [(Float, Float)] -> Int
countallvalid :: [(Float, Float)] -> Int
countallvalid input = product $ map (length . uncurry validTimes) input

