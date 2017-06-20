singleItemLists :: [Maybe Int] -> [Maybe Int]
singleItemLists xs = xs >>= \x -> case x of Just x -> [Just x]; _ -> []

main :: IO ()
main = putStrLn $ show $ singleItemLists [Just 1, Just 2, Nothing]
