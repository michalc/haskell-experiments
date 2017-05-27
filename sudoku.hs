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

isSolved :: State GridState Bool
isSolved = get >>= return . all (((==) 1) . length)

iteration :: State GridState [()]
iteration = flip untilM isSolved $ do
  mapM_ iterationRow [0..8]
  mapM_ iterationColumn [0..8]
  mapM_ iterationCell [(i,j) | i <- [0..2], j <- [0..2]]

iterationRow :: Int -> State GridState ()
iterationRow i = modify $ \matrix ->
  replaceSubmatrix sub matrix (reducePotentials $ submatrix sub matrix)
  where
    sub = (i, 0, i + 1, 9)

iterationColumn :: Int -> State GridState ()
iterationColumn i = modify $ \matrix ->
  replaceSubmatrix sub matrix (reducePotentials $ submatrix sub matrix)
  where
    sub = (0, i, 9, i + 1)

iterationCell :: (Int, Int) -> State GridState ()
iterationCell (i, j) = modify $ \matrix ->
  replaceSubmatrix sub matrix (reducePotentials $ submatrix sub matrix)
  where
    sub = (i * 3, j * 3, (i+1) * 3, (j+1) * 3)

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
certains = map head . filter (((==) 1) . length)


--- Matrix / utilitiy operations ---

replaceSubmatrix :: (Int, Int, Int, Int) -> [a] -> [a] -> [a]
replaceSubmatrix (i, j, k, l) matrix newSubmatrix = map replace $ zip matrix [0..]
  where
    replace x_i
      | isInSubmatrix (i, j, k, l) (snd x_i) = newSubmatrix !! (indexInSubmatrix $ snd x_i)
      | otherwise                             = matrix       !! snd x_i
    indexInSubmatrix  i_parent = ((rowInSubmatrix i_parent) * submatrixWidth) + columnInSubmatrix i_parent
    rowInSubmatrix    i_parent = rowOfIndex    (i_parent - i * 9 - j)
    columnInSubmatrix i_parent = columnOfIndex (i_parent - i * 9 - j)
    submatrixWidth             = l - j
    submatrixHeight            = k - i

submatrix :: (Int, Int, Int, Int) -> [a] -> [a]
submatrix (i, j, k, l) matrix = [
    fst x_i | x_i <- zip matrix [0..], 
    isInSubmatrix (i, j, k, l) $ snd x_i
  ]

isInSubmatrix :: (Int, Int, Int, Int) -> Int -> Bool
isInSubmatrix (i, j, k, l) index = isBetween i k (rowOfIndex index) && isBetween j l (columnOfIndex index)

rowOfIndex :: Int -> Int
rowOfIndex i = i `quot` 9

columnOfIndex :: Int -> Int
columnOfIndex i = i `mod` 9

cellOfIndex :: Int -> (Int, Int)
cellOfIndex i = ((rowOfIndex i) `quot` 3, (columnOfIndex i) `quot` 3)

isBetween :: Int -> Int -> Int -> Bool
isBetween a b x = a <= x && x < b

