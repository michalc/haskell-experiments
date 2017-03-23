import Data.List.Extras.LazyLength

main :: IO ()
main = putStrLn $ show $ tester [1,2,3] 

tester xs
  | lengthBound 4 (>) xs = Nothing
  | otherwise            = Just 4
 