data NBaum a = NBlatt a | NKnoten a [NBaum a] deriving (Show)


entferneBlaetter :: NBaum a -> NBaum a
entferneBlaetter (NKnoten s xs) = NKnoten s [entferneBlaetter x | x <- xs, not $ isLeaf x]
entferneBlaetter x              = x

isLeaf :: NBaum a -> Bool
isLeaf (NBlatt _)     = True
isLeaf (NKnoten _ []) = True
isLeaf s              = False

main :: IO ()
-- main = putStrLn $ show $ entferneBlaetter $ NKnoten "hello" [NBlatt "leaf", NKnoten "empty" [], NKnoten "stuff" [NBlatt "leaf", NKnoten "stuff" [NBlatt "leaf"]]]
main = putStrLn $ show $ entferneBlaetter $ NKnoten "test" []
