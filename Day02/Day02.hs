module Day02
( testInput
, solve1
, solve2
)
where
import Data.Char


secondGameScore :: [Char] -> Int
secondGameScore (otherHand:' ':outcome:[])
  | outcome == 'X' = (ord otherHand - ord 'A' - 1) `mod` 3 + 1
  | outcome == 'Y' = (ord otherHand - ord 'A' + 4)
  | otherwise = 6 + (ord otherHand - ord 'A' + 1) `mod` 3 + 1
secondGameScore _ = undefined

handScore :: Char -> Int
handScore 'X' = 1
handScore 'Y' = 2
handScore 'Z' = 3
handScore _ = undefined

gameScore :: [Char] -> Int
gameScore (otherHand:' ':myHand:[])
  | ord otherHand == ord myHand - ord 'X' + ord 'A' = 3 + handScore myHand
  | otherHand == 'A' && myHand == 'Y' ||
    otherHand == 'B' && myHand == 'Z' ||
    otherHand == 'C' && myHand == 'X' = 6 + handScore myHand
  | otherwise = handScore myHand
gameScore _ = undefined
  
testInput :: String
testInput = "A Y\n\
\B X\n\
\C Z"

solve1, solve2 :: String -> Int
solve1 = sum . map gameScore . lines
solve2 = sum . map secondGameScore . lines