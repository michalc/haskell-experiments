import qualified Data.Map as Map
import Control.Monad.State.Strict

main = print $ runMyMemo getNumberOfPaths (20, 20)

getNumberOfPaths :: (Integer, Integer) -> MyMemo (Integer, Integer) Integer
getNumberOfPaths (0, _) = return 1
getNumberOfPaths (_, 0) = return 1
getNumberOfPaths (x, y) = do
  n1 <- myMemo getNumberOfPaths (x-1,y)
  n2 <- myMemo getNumberOfPaths (x,y-1)
  return (n1 + n2)

-------

type MyMemo a b = State (Map.Map a b) b

myMemo :: Ord a => (a -> MyMemo a b) -> a -> MyMemo a b
myMemo f x = gets (Map.lookup x) >>= maybe y' return
  where
    y' = f x >>= \y -> modify (Map.insert x y) >> return y

runMyMemo :: Ord a => (a -> MyMemo a b) -> a -> b
runMyMemo f x = evalState (f x) Map.empty
