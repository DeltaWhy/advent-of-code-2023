import Data.Char
import Data.List
import Data.List.Split

main = do
  input <- parseInput <$> getContents
  print $ partOne input
  print $ partTwo input

testInput = parseInput $
  unlines
    [
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]

data Round = Round {red::Int, green::Int, blue::Int} deriving (Show, Eq)
type Game = [Round]

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:y:xs) = (x,y):pairs xs
pairs [x] = error "odd list"

parseInput = let
  trimmed = drop 2 . dropWhile (/= ':')
  splitted = splitOn "; " 
  blocks = words . filter (/= ',')
  foldingFn :: Round -> (String,String) -> Round
  foldingFn acc (x,y) | y == "red" = acc { red = read x }
    | y == "green" = acc { green = read x }
    | y == "blue" = acc { blue = read x }
    | otherwise = error "bad color"
  makeRound :: [(String,String)] -> Round
  makeRound = foldl foldingFn (Round 0 0 0)
  in map (map (makeRound . pairs . blocks) . splitted . trimmed) . lines


possible :: Game -> Bool
possible [] = True
possible (r:rs) = red r <= 12 && green r <= 13 && blue r <= 14 && possible rs

partOne :: [Game] -> Int
partOne input = sum $ [i | (i, g) <- zip [1..] input, possible g]


power :: Round -> Int
power (Round r g b) = r * g * b

minCubes :: Game -> Round
minCubes = let
  foldingFn :: Round -> Round -> Round
  foldingFn (Round maxR maxG maxB) (Round r g b) = Round (max maxR r) (max maxG g) (max maxB b)
  in foldl foldingFn (Round 0 0 0)


partTwo :: [Game] -> Int
partTwo input = sum $ map (power . minCubes) input
