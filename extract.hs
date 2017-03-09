import Data.List (intersperse)
import Data.List.Split (splitOn)
import Data.Char (toUpper, toLower)
import Control.Arrow ((>>>))

main = readFile "un.txt" >>= (
    filter (flip elem allowedCharacters) >>>
    map toLower >>>
    splitOn " " >>>
    zip [0..] >>>
    filter (every 7) >>>
    map convertWord >>>
    concat >>>
    writeFile "un_every_7th.txt"
  )

capitalized :: String -> String
capitalized (head:tail) = toUpper head : map toLower tail
capitalized [] = []

convertWord :: (Int, String) -> String
convertWord (i, word) 
  | mod (i + 1) (15 * 5) == 0  = word ++ ".\n\n"
  | mod (i + 1) (15 * 5) == 1  = capitalized word ++ " "
  | mod (i + 1) 15       == 0  = word ++ ",\n"
  | otherwise                  = word ++ " "

every :: Int -> (Int, String) -> Bool
every m (x, _) = (mod x m) == 0

allowedCharacters = [' '] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
