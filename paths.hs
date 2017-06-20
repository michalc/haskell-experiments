{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Memo

main = print $ startEvalMemo (getNumberOfPaths 30 30)

getNumberOfPaths :: (MonadMemo (Integer, Integer) Integer m) => Integer -> Integer -> m Integer
getNumberOfPaths 0 _ = return 1
getNumberOfPaths _ 0 = return 1
getNumberOfPaths x y = do
  n1 <- for2 memo getNumberOfPaths (x-1) y
  n2 <- for2 memo getNumberOfPaths x (y-1)
  return (n1 + n2)
