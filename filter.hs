
main :: IO ()
main = putStrLn $ show $ myFilter [[1,2,3], [5,7,8,12,34], [2,4,6,8]]

myFilter :: [[Integer]] -> [[Integer]]
myFilter =  (filter $ (>= 2) . length) . (map $ filter (>= 6))