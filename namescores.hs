import Data.Char
import Data.List

main = do
  nameStr <- readFile "names.txt"
  print $ totalNameScore nameStr

totalNameScore :: String -> Int
totalNameScore nameStr =
  let names = sort . cwords $ filter (`elem` (',':['A'..'Z'])) nameStr
  in sum . zipWith (\x y -> (score x)*y) names $ [1..length names]

cwords :: String -> [String]
cwords s = 
  case span (/=',') s of 
    (w, []) -> [w]
    (w, rest) -> w:(cwords (tail rest))

score :: String -> Int
score = sum . map (\x -> (ord x) - 64) 