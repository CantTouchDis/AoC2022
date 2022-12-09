module Day06
( testInput
, solve1
, solve2
)
where
import Data.List

testInput :: String
testInput = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

findMarker :: Int -> String -> Int
findMarker n = find' n
  where
  find' n' s@(_:rest)
    | n == (length . nub . sort . take n) s = n'
    | otherwise                 = find' (n' + 1) (rest)
  find' _ [] = undefined


solve1, solve2 :: String -> Int
solve1 = findMarker 4 
solve2 = findMarker 14