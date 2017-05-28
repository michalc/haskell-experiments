import Control.Lens
import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.List
import Data.List.Split
import Data.Maybe

data SudokuValue = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 deriving (Eq, Enum)
instance Show SudokuValue where
  show s = show $ fromJust (s `elemIndex` [S1 ..]) + 1

type MatrixIndex = Int

initial = [
    Nothing, Nothing, Just S3,   Nothing, Nothing, Just S7,   Just S1, Nothing, Nothing,
    Nothing, Just S4, Just S1,   Nothing, Just S2, Nothing,   Nothing, Nothing, Just S5,
    Just S9, Nothing, Just S6,   Nothing, Just S5, Just S1,   Just S2, Just S3, Nothing,

    Just S6, Nothing, Nothing,   Just S5, Just S8, Nothing,   Just S9, Nothing, Nothing,
    Nothing, Nothing, Just S8,   Nothing, Nothing, Nothing,   Just S7, Nothing, Nothing,
    Nothing, Nothing, Just S2,   Nothing, Just S4, Just S9,   Nothing, Nothing, Just S6,

    Nothing, Just S2, Just S9,   Just S8, Just S7, Nothing,   Just S3, Nothing, Just S1,
    Just S8, Nothing, Nothing,   Nothing, Just S6, Nothing,   Just S5, Nothing, Nothing,
    Nothing, Nothing, Just S5,   Just S9, Nothing, Nothing,   Just S4, Nothing, Nothing
  ]

main :: IO ()
main = putStrLn $ niceString $ execState iteration $ map toPotential initial
  where
    toPotential Nothing  = [S1 ..]
    toPotential (Just x) = [x]

niceString :: [[SudokuValue]] -> String
niceString matrix = intercalate "\n" $ chunksOf 18 asStrings
  where
    asStrings = intercalate " " $ map (show . head) matrix

iteration :: State [[SudokuValue]] ()
iteration = flip untilM_ isSolved $ mapM_ iterationGroup groups
  where
    iterationGroup matrix = partsOf (traversed . indices (`elem` matrix)) %= reducePotentials
    isSolved = get >>= return . all (((==) 1) . length)

groups :: [[MatrixIndex]]
groups = rows ++ columns ++ cells
  where
    rows = chunksOf 9 [0..80]
    columns = transpose rows
    cells = concatMap (map concat . chunksOf 3) $ transpose $ map (chunksOf 3) columns

reducePotentials :: [[SudokuValue]] -> [[SudokuValue]]
reducePotentials subMatrix = map (withoutPotential) subMatrix 
  where
    withoutPotential [x] = [x]
    withoutPotential  xs = xs \\ [x | [x] <- subMatrix]
