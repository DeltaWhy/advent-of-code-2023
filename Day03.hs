import Data.Char
import Data.List
import Data.List.Split

main = do
  input <- parseInput <$> getContents
  print $ partOne input
  print $ partTwo input

testInput =
  parseInput $
    unlines
      [ "467..114..",
        "...*......",
        "..35..633.",
        "......#...",
        "617*......",
        ".....+.58.",
        "..592.....",
        "......755.",
        "...$.*....",
        ".664.598.."
      ]

type Point = (Int, Int)

type Symbol = (Char, Point)

type PartNum = (Int, Point)

data Schematic = Schematic {symbols :: [Symbol], partNums :: [PartNum]} deriving (Show)

readInt :: String -> [(Int, String)]
readInt line =
  let a = takeWhile isDigit line
      b = dropWhile isDigit line
  in
    if null a then [] else [(read a, b)]
  
numsInLine :: String -> [(Int, Int)]
numsInLine line = let
    numsInLine' :: Int -> String -> [(Int, Int)] -> [(Int, Int)]
    numsInLine' x line acc =
      case readInt line :: [(Int, String)] of
        [(n, remainder)] -> numsInLine' (x + length line - length remainder) remainder ((n, x) : acc)
        _ -> if null line then acc else numsInLine' (x + 1) (tail line) acc
  in numsInLine' 0 line []

parseInput :: String -> Schematic
parseInput input =
  let isSymbol c = case c of
        '.' -> False
        x | isDigit x -> False
        _ -> True
      enumerate = zip [0 ..]
      syms = [(c, (x, y)) | (y, line) <- enumerate $ lines input, (x, c) <- enumerate line, isSymbol c]
      nums = [(n, (x, y)) | (y, line) <- enumerate $ lines input, (n, x) <- numsInLine line]
   in Schematic syms nums

hasAdjacentSymbol :: [Symbol] -> PartNum -> Bool
hasAdjacentSymbol syms (n, (x, y)) = let
  endX = x + length (show n) - 1
  in any (\(_, (sx, sy)) -> sx >= x - 1 && sx <= endX + 1 && abs (sy - y) <= 1) syms

partOne :: Schematic -> Int
partOne (Schematic syms nums) = 
  sum $ [n | num@(n, _) <- nums, hasAdjacentSymbol syms num]

numsAdjacentToSymbol :: [PartNum] -> Symbol -> [PartNum]
numsAdjacentToSymbol nums sym = filter (hasAdjacentSymbol [sym]) nums

gearValue nums sym =
  let
    adjacents = numsAdjacentToSymbol nums sym
  in
    case adjacents of
      [(n1, _), (n2, _)] -> n1 * n2
      _ -> 0

partTwo :: Schematic -> Int
partTwo (Schematic syms nums) = sum $ map (gearValue nums) syms
