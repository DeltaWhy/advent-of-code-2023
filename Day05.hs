import Data.Char
import Data.List
import Data.List.Extra

main = do
  input <- parseInput <$> getContents
  print $ partOne input
  -- print $ partTwo input

testInput = parseInput $
  unlines
    [
      "seeds: 79 14 55 13",
      "",
      "seed-to-soil map:",
      "50 98 2",
      "52 50 48",
      "",
      "soil-to-fertilizer map:",
      "0 15 37",
      "37 52 2",
      "39 0 15",
      "",
      "fertilizer-to-water map:",
      "49 53 8",
      "0 11 42",
      "42 0 7",
      "57 7 4",
      "",
      "water-to-light map:",
      "88 18 7",
      "18 25 70",
      "",
      "light-to-temperature map:",
      "45 77 23",
      "81 45 19",
      "68 64 13",
      "",
      "temperature-to-humidity map:",
      "0 69 1",
      "1 0 69",
      "",
      "humidity-to-location map:",
      "60 56 37",
      "56 93 4"
    ]

data RangeEntry = RangeEntry { destStart :: Int, sourceStart :: Int, rangeLength :: Int } deriving (Show)
type RangeMap = [RangeEntry]

parseInput input =
  let
    groups = split null $ lines input
    ([seedLine]:mapGroups) = groups
    seeds = map read $ tail $ words seedLine :: [Int]
    parseEntry line = let [x, y, z] = map read $ words line in RangeEntry x y z
    parseGroup group = map parseEntry $ tail group
  in (seeds, map parseGroup mapGroups)

rangeLookup :: RangeMap -> Int -> Int
rangeLookup [] x = x
rangeLookup (e:rest) x =
  if x >= sourceStart e && x < (sourceStart e + rangeLength e)
  then x - sourceStart e + destStart e
  else rangeLookup rest x

partOne :: ([Int], [RangeMap]) -> Int
partOne (seeds, maps) =
  let
    mapFns = map rangeLookup maps
    chainFn = foldl1 (.) $ reverse mapFns
    locations = map chainFn seeds
  in
    minimum locations

partTwo input = undefined
