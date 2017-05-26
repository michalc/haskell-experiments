-- data Person = Person String Person

data Person = Person String (Maybe Person) deriving (Show)

main :: IO ()
main = putStrLn $ show $ showGrandmother $ Person "Me" $ Just $ Person "mother" $ Just $ Person "grandmother" Nothing

showMother :: Person -> Maybe Person
showMother (Person _ (Just mother)) = Just mother
showMother _                        = Nothing

showGrandmother :: Person -> Maybe Person
showGrandmother x = showMother x >>= showMother

-- a = Person "Test" b
-- b = Person "Root" b