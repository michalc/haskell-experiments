import Control.Lens
import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe

data SudokuValue = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 deriving (Eq, Enum)
instance Show SudokuValue where
  show s = show $ fromJust (s `elemIndex` [S1 ..]) + 1

type Potentials = Int
type MatrixIndex = Int

allPotentials :: Potentials
allPotentials = foldr (.|.) zeroBits $ map (bit.fromEnum) [S1 ..]

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
main = putStrLn $ niceString $ untilStable (execState groupTransforms) $ map toPotential initial
  where
    niceString s
      | isSolved s = intercalate "\n" $ (chunksOf 18) $ unwords $ map (show.toSudokuValue) s
      | otherwise  = "Not solveable"
    isSolved = all ((1 ==) . popCount)
    toPotential Nothing  = allPotentials
    toPotential (Just x) = toPotentials x

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f a
  | a' == a   = a
  | otherwise = untilStable f a'
  where a' = f a

groups :: [[MatrixIndex]]
groups = rows ++ columns ++ cells
  where
    rows = chunksOf 9 [0..80]
    columns = transpose rows
    cells = concatMap (map concat . chunksOf 3) $ transpose $ map (chunksOf 3) columns

groupTransforms :: State [Potentials] ()
groupTransforms = mapM_ groupTransform groups
  where
    groupTransform group = partsOf (traversed . indices (`elem` group)) %= reducePotentials

reducePotentials :: [Potentials] -> [Potentials]
reducePotentials subMatrix = map withoutPotential subMatrix 
  where
    withoutPotential xs
      | popCount xs == 1 = xs
      | otherwise        = xs .&. (complement certainties)
    certainties          = foldr (.|.) zeroBits $ filter ((1 ==) . popCount) subMatrix

toPotentials :: SudokuValue -> Potentials
toPotentials = bit . fromEnum

toSudokuValue :: Potentials -> SudokuValue
toSudokuValue x = toEnum $ countTrailingZeros x