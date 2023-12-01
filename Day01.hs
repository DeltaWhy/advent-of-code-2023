import Data.Char
import Data.List

main = do
  input <- getContents
  let output = solvePart1 $ parseInput input
  print output
  let output2 = solvePart2 $ parseInput input
  print output2

testInput =
  unlines
    [ "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    ]

parseInput = lines

calibration :: String -> Int
calibration x = 10 * digitToInt (head digits) + digitToInt (last digits)
  where
    digits = filter isDigit x

solvePart1 :: [String] -> Int
solvePart1 input = sum $ map calibration input

testInput2 =
  unlines
    [ "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    ]

digitValues =
  [ ("0", 0),
    ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9),
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

digitSubstrs :: String -> [Int]
digitSubstrs x = [v | t <- tails x, (k, v) <- digitValues, k `isPrefixOf` t]

calibration2 x =
  let firstDigit = head $ digitSubstrs x
      lastDigit = last $ digitSubstrs x
   in 10 * firstDigit + lastDigit

solvePart2 :: [String] -> Int
solvePart2 input = sum $ map calibration2 input
