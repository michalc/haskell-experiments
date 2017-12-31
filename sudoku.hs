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

-- Sudoku solver, solving a hard-coded initial grid
-- Creates a matrix of lists, each a list of "potential", values for each cell.
-- Then iteratively removes potentials from each row, column, and 3x3 cell
-- until the the removal process doesn't remove anything
main :: IO ()
main = putStrLn $ niceString $ untilStable (execState groupTransforms) $ map toPotential initial
  where
    niceString s
      | isSolved s = intercalate "\n" $ (chunksOf 18) $ unwords $ map (show . head) s
      | otherwise  = "Not solveable"
    isSolved = all ((1 ==) . length)
    toPotential Nothing  = [S1 ..]
    toPotential (Just x) = [x]

-- Repeatedly applies the passed function until it does not change its input
untilStable :: Eq a => (a -> a) -> a -> a
untilStable f a
  | a' == a   = a
  | otherwise = untilStable f a'
  where a' = f a

-- Lists of each index in each sudoku "group", i.e. each row, column or 3x3 cell
sudokuGroups :: [[MatrixIndex]]
sudokuGroups = rows ++ columns ++ cells
  where
    rows = chunksOf 9 [0..80]
    columns = transpose rows
    cells = concatMap (map concat . chunksOf 3) $ transpose $ map (chunksOf 3) columns

-- State transform running one iteration of reducePotentials for each sudokuGroup
groupTransforms :: State [[SudokuValue]] ()
groupTransforms = mapM_ groupTransform sudokuGroups
  where
    groupTransform group = partsOf (traversed . indices (`elem` group)) %= reducePotentials

-- For each "certainty" in the lists passed (i.e. list of single value)
-- removes this "certainly" from the other lists
reducePotentials :: [[SudokuValue]] -> [[SudokuValue]]
reducePotentials subMatrix = map withoutPotential subMatrix 
  where
    withoutPotential [x] = [x]
    withoutPotential  xs = xs \\ certainties
    certainties          = subMatrix >>= \x -> case x of [x] -> [x] ; _ -> []
