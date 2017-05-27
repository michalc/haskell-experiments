import Control.Monad.State.Strict
import Data.List

-- Convert input cells to lists of potentials.
-- The known cells are lists of 1, the unknowns are lists of 1 to 9
-- First, they are all numbers 1 to 9
-- First pass at iteration loop
--- Go through each row and column, and cell, find all the ones that are certain, and remove those from tne others
--- For tricky ones, suspect it's not enough!

type GridState = [[Int]]

initial = [
    Nothing, Nothing,  Just 3,   Nothing, Nothing,  Just 7,    Just 1, Nothing, Nothing,
    Nothing,  Just 1,  Just 1,   Nothing,  Just 2, Nothing,   Nothing, Nothing,  Just 5,
     Just 9, Nothing,  Just 6,   Nothing,  Just 5,  Just 1,    Just 2,  Just 3, Nothing,

     Just 6, Nothing, Nothing,    Just 5,  Just 8, Nothing,    Just 9, Nothing, Nothing,
    Nothing, Nothing,  Just 8,   Nothing, Nothing, Nothing,    Just 7, Nothing, Nothing,
    Nothing, Nothing,  Just 2,   Nothing,  Just 4,  Just 9,   Nothing, Nothing,  Just 6,

    Nothing,  Just 2,  Just 9,    Just 8,  Just 7, Nothing,    Just 3, Nothing,  Just 1,
     Just 8, Nothing, Nothing,   Nothing,  Just 6, Nothing,    Just 5, Nothing, Nothing,
    Nothing, Nothing,  Just 5,    Just 9, Nothing, Nothing,    Just 4, Nothing, Nothing
  ]



main :: IO ()
main = putStrLn $ show $ snd $ runState iteration $ toPotentials initial

nothing :: i -> State GridState ()
nothing i = state $ \s -> ((), s)

getRowInState :: Int -> State GridState [[Int]]
getRowInState i = state $ \s -> (row i s, s)

replaceRowInState :: Int -> [[Int]] -> State GridState ()
replaceRowInState i newRow = state $ \s -> ((), replaceRow i s newRow)

iteration :: State GridState ()
iteration = do
  row <- getRowInState 0
  replaceRowInState 0 $ reducePotentials row
  nothing 1


-- Dealing with "potentials" -- 

toPotentials :: [Maybe Int] -> [[Int]]
toPotentials matrix = map toPotential matrix

toPotential :: Maybe Int -> [Int]
toPotential Nothing  = [1..9]
toPotential (Just x) = [x]

reducePotentials :: (Eq a) => [[a]] -> [[a]]
reducePotentials subMatrix = map (withoutPotential) subMatrix 
  where
    withoutPotential [x] = [x]
    withoutPotential  xs = xs \\ certains subMatrix

certains :: [[a]] -> [a]
certains subMatrix = map (\ xs -> xs !! 0) $ filter (\xs -> length xs == 1) subMatrix


--- Matrix / utilitiy Operations ---

row :: Int -> [a] -> [a]
row i matrix = [fst x_i | x_i <- indexed, rowOfIndex (snd x_i) == i]
  where
    indexed = zip matrix [0..]

replaceRow :: Int -> [a] -> [a] -> [a]
replaceRow i matrix newRow = map replace indexed
  where
    indexed = zip matrix [0..]
    replace x_i
      | rowOfIndex (snd x_i) == i = newRow !! (columnOfIndex $ snd x_i)
      | otherwise                 = matrix !! snd x_i

replaceColumn :: Int -> [a] -> [a] -> [a]
replaceColumn i matrix newColumn = map replace indexed
  where
    indexed = zip matrix [0..]
    replace x_i
      | columnOfIndex (snd x_i) == i = newColumn !! (rowOfIndex $ snd x_i)
      | otherwise                    = matrix    !! snd x_i

column :: Int -> [a] -> [a]
column i matrix = [fst x_i | x_i <- indexed, columnOfIndex (snd x_i) == i]
  where
    indexed = zip matrix [0..]

cell :: Int -> Int -> [a] -> [a]
cell i j matrix = [
    fst x_i |
    x_i <- indexed,
    isBetween (i * 3) ((i + 1) * 3) (rowOfIndex (snd x_i)) 
    && isBetween (i * 3) ((i + 1) * 3) (columnOfIndex (snd x_i)) 
  ]
  where
    indexed = zip matrix [0..]

rowOfIndex :: Int -> Int
rowOfIndex i = i `quot` 9

columnOfIndex :: Int -> Int
columnOfIndex i = i `mod` 9

isBetween :: Int -> Int -> Int -> Bool
isBetween a b x = a <= x && x < b
