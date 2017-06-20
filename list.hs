import Control.Monad

main :: IO ()
main = putStrLn . show $ [1,2,3,4,5] >>= return . mapper

-- main = putStrLn . show $ [1,2,3,4,5] >>= \x -> if predi x then [x] else []
-- main = putStrLn . show $ do 
--   x <- [1,2,3,4,5] 
--   guard $ predi x
--   [x]

predi = \x -> x `mod` 2 == 0 

mapper = \x -> 2 * x
