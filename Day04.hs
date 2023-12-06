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
      "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    ]


type Card = ([Int], [Int])

parseInput :: String -> [Card]
parseInput input = let
  trimmed = drop 2 . dropWhile (/= ':')
  splitted = splitOn " | " 
  nums = map read . words
  parseLine line = 
    case (splitted . trimmed) line of
      [winning, have] -> (nums winning, nums have)
      _ -> error "bad input"
  in map parseLine $ lines input


cardValue (winning, have) = case length (winning `intersect` have) of
  0 -> 0
  n -> 2 ^ (n - 1)

partOne :: [Card] -> Int
partOne input = sum $ map cardValue input

cardFold cards = cardFold' cards (map (const 1) cards) 0
  where
    cardFold' cards copies i
      | i >= length cards = copies
      | otherwise =
        let
          (winning, have) = cards !! i
          score = length (winning `intersect` have)
          n = copies !! i
          copies' = map (\(j, x) -> if j > i && j <= i + score then n + x else x) (zip [0..] copies)
        in cardFold' cards copies' (i + 1)

partTwo :: [Card] -> Int
partTwo input = sum $ cardFold input
