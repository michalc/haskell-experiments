import Control.Lens
import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.List
import Data.List.Split

type GridState = [[Int]]

initial = [
    Nothing, Nothing,  Just 3,   Nothing, Nothing,  Just 7,    Just 1, Nothing, Nothing,
    Nothing,  Just 4,  Just 1,   Nothing,  Just 2, Nothing,   Nothing, Nothing,  Just 5,
     Just 9, Nothing,  Just 6,   Nothing,  Just 5,  Just 1,    Just 2,  Just 3, Nothing,

     Just 6, Nothing, Nothing,    Just 5,  Just 8, Nothing,    Just 9, Nothing, Nothing,
    Nothing, Nothing,  Just 8,   Nothing, Nothing, Nothing,    Just 7, Nothing, Nothing,
    Nothing, Nothing,  Just 2,   Nothing,  Just 4,  Just 9,   Nothing, Nothing,  Just 6,

    Nothing,  Just 2,  Just 9,    Just 8,  Just 7, Nothing,    Just 3, Nothing,  Just 1,
     Just 8, Nothing, Nothing,   Nothing,  Just 6, Nothing,    Just 5, Nothing, Nothing,
    Nothing, Nothing,  Just 5,    Just 9, Nothing, Nothing,    Just 4, Nothing, Nothing
  ]



main :: IO ()
main = putStrLn $ niceString $ snd $ runState iteration $ map toPotential initial
  where
    toPotential Nothing  = [1..9]
    toPotential (Just x) = [x]

niceString :: [[Int]] -> String
niceString matrix = intercalate "\n" $ chunksOf 18 asStrings
  where
    asStrings = intercalate " " $ map (show . head) matrix

iteration :: State GridState ()
iteration = flip untilM_ isSolved $ mapM_ iterationGroup groups
  where
    isSolved = get >>= return . all (((==) 1) . length)

groups :: [[Int]]
groups = rows ++ columns ++ cells
  where
    rows = chunksOf 9 [0..80]
    columns = transpose rows
    cells = concatMap (map concat . chunksOf 3) $ transpose $ map (chunksOf 3) columns

iterationGroup :: [Int] -> State GridState ()
iterationGroup is = partsOf (traversed . indices (`elem` is)) %= reducePotentials

reducePotentials :: (Eq a) => [[a]] -> [[a]]
reducePotentials subMatrix = map (withoutPotential) subMatrix 
  where
    withoutPotential [x] = [x]
    withoutPotential  xs = xs \\ [x | [x] <- subMatrix]
