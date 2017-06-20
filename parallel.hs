import Control.Parallel.Strategies
import Data.List.Split

elementToFind = 1
numElements = 1000000000 * 2

hasPropertyX :: Integer -> Bool
hasPropertyX i = i == elementToFind

chunked = chunksOf 100000000 [1..numElements]

main :: IO ()
main = putStrLn $ show $ or $ using [any hasPropertyX xs | xs <- chunked] (evalList rpar)

