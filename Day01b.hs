import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

main = do
  input <- getContents
  let (Just output) = solvePart1 $ parseInput input
  print output
  let (Just output2) = solvePart2 $ parseInput input
  print output2

testInput =
  unlines
    [ "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    ]

parseInput = lines

firstDigit1 :: String -> Maybe Int
firstDigit1 "" = Nothing
firstDigit1 (c:cs) = if isDigit c then return (digitToInt c) else firstDigit1 cs

lastDigit1 :: String -> Maybe Int
lastDigit1 =
  let maybeDigit c = if isDigit c then Just (digitToInt c) else Nothing
      f c Nothing = maybeDigit c
      f _ m = m
  in foldr f Nothing

calibration :: String -> Maybe Int
calibration x = do
    let digits = filter isDigit x
        firstDigit "" = Nothing
        firstDigit (c:cs) = if isDigit c then return (digitToInt c) else firstDigit cs
    f <- firstDigit x
    l <- lastDigit1 x
    return $ 10 * f + l
  where
    digits = filter isDigit x

solvePart1 :: [String] -> Maybe Int
solvePart1 input = sum <$> mapM calibration input

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

valueAt :: String -> Maybe Int
valueAt "" = Nothing
valueAt (c : cs) | isDigit c = return $ digitToInt c
valueAt s
  | "zero" `isPrefixOf` s = return 0
  | "one" `isPrefixOf` s = return 1
  | "two" `isPrefixOf` s = return 2
  | "three" `isPrefixOf` s = return 3
  | "four" `isPrefixOf` s = return 4
  | "five" `isPrefixOf` s = return 5
  | "six" `isPrefixOf` s = return 6
  | "seven" `isPrefixOf` s = return 7
  | "eight" `isPrefixOf` s = return 8
  | "nine" `isPrefixOf` s = return 9
  | otherwise = Nothing

firstDigit2 :: String -> Maybe Int
firstDigit2 s = case valueAt s of
  Just x -> Just x
  Nothing -> firstDigit2 $ tail s

lastDigit2 :: String -> Maybe Int
lastDigit2 =
  let
    f [] = Nothing
    f (t:ts) = case valueAt t of
      Just x -> Just x
      Nothing -> f ts
  in f . reverse . tails

digitSubstrs :: String -> [Int]
digitSubstrs x = join $ map (maybeToList . valueAt) (tails x)

calibration2 :: String -> Maybe Int
calibration2 x = do
  f <- firstDigit2 x
  l <- lastDigit2 x
  return $ 10 * f + l

solvePart2 :: [String] -> Maybe Int
solvePart2 input = sum <$> mapM calibration2 input
