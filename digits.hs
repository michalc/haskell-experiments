-- import Data.Char

-- digitToIntMay :: Char -> Maybe Int
-- digitToIntMay '.' = Nothing
-- digitToIntMay  x  = Just $ digitToInt x

-- main = putStrLn $ show $ fmap digitToIntMay "13.2..2"


import Data.Char

charToMaybeInt :: Char -> Maybe Int
charToMaybeInt  x 
 | isDigit x = Just $ digitToInt x
 | otherwise = Nothing

main = putStrLn $ show $ fmap charToMaybeInt "13.2..2"
