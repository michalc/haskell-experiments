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
main = putStrLn $ niceString $ snd $ runState iteration $ toPotentials initial

niceString :: [[Int]] -> String
niceString matrix = intercalate "\n" $ chunksOf 18 asStrings
  where
    asStrings = intercalate " " $ map (show . head) matrix

isNotSolved :: State GridState Bool
isNotSolved = get >>= return . any (((<) 1) . length)

iteration :: State GridState [()]
iteration = whileM isNotSolved $ do
  foldM (\_ -> iterationRow) () [0..8]
  foldM (\_ -> iterationColumn) () [0..8]
  foldM (\_ -> iterationCell) () [(i,j) | i <- [0..2], j <- [0..2]]

iterationRow :: Int -> State GridState ()
iterationRow i = modify $ \matrix 
  -> replaceRow i matrix (reducePotentials $ row i matrix)

iterationColumn :: Int -> State GridState ()
iterationColumn i = modify $ \matrix 
  -> replaceColumn i matrix (reducePotentials $ column i matrix)

iterationCell :: (Int, Int) -> State GridState ()
iterationCell (i, j) = modify $ \matrix
  -> replaceCell (i,j) matrix (reducePotentials $ cell (i,j) matrix)


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
    withoutPotential  xs = xs \\ (certains subMatrix)

certains :: [[a]] -> [a]
certains subMatrix = map head $ filter (\xs -> length xs == 1) subMatrix


--- Matrix / utilitiy operations ---

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

replaceCell :: (Int, Int) -> [a] -> [a] -> [a]
replaceCell (i, j) matrix newCell = map replace indexed
  where
    indexed = zip matrix [0..]
    replace x_i
      | cellOfIndex (snd x_i) == (i, j) = newCell !! (indexInNewCell $ snd x_i)
      | otherwise                       = matrix  !! snd x_i

    indexInNewCell i_parent = (rowInCell i_parent) * 3 + columnInCell i_parent 
    rowInCell      i_parent = (i_parent - i * 9 * 3) `quot` 9
    columnInCell   i_parent = i_parent `mod` 3

column :: Int -> [a] -> [a]
column i matrix = [fst x_i | x_i <- indexed, columnOfIndex (snd x_i) == i]
  where
    indexed = zip matrix [0..]

cell :: (Int, Int) -> [a] -> [a]
cell (i,j) matrix = [fst x_i | x_i <- indexed, cellOfIndex (snd x_i) == (i, j)]
  where
    indexed = zip matrix [0..]

rowOfIndex :: Int -> Int
rowOfIndex i = i `quot` 9

columnOfIndex :: Int -> Int
columnOfIndex i = i `mod` 9

cellOfIndex :: Int -> (Int, Int)
cellOfIndex i = ((rowOfIndex i) `quot` 3, (columnOfIndex i) `quot` 3)
