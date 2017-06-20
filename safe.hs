
isElemChar :: Char -> Bool
isElemChar x = elem x ['A'..'Z'] || elem x ['a'..'z']

main = print $ isElemChar 5