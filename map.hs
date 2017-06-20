import qualified Data.Map as Map

m = Map.empty

main = print $ Map.lookup 1 $ if Map.member 1 m then m else Map.insert 1 2 m
