{-# LANGUAGE QuasiQuotes #-}
module Day10
( testInput
, solve1
, solve2
)
where
import Literal

testInput :: String
testInput = [litFile|data/test10.txt|]

data Instruction = ADD Int | NOOP deriving (Show)

instructionFrom :: String -> Instruction
instructionFrom "noop" = NOOP
instructionFrom s = ADD (read $ (words s) !! 1)

processInstruction :: Int -> Int -> Instruction -> [(Int, Int)]
processInstruction c v NOOP = [(c+1, v)]
processInstruction c v (ADD x) = [(c+1, v), (c+2, v+x)]

sumSignalStrengths :: Int -> Int -> [(Int, Int)] -> [Int]
sumSignalStrengths start step l = sumStrengths' start $ dropWhile ((/=start) . fst) l
  where
  sumStrengths' _ [] = []
  sumStrengths' n (a:r) = (n * (snd a)):(sumStrengths' (n + step) (dropWhile ((/=n + step) . fst) r))

drawCRT :: Int -> [(Int, Int)] -> String
drawCRT n = draw'
  where
  continue [] = "\n"
  continue l@((c, _):_) = if mod (c - 1) n == 0 then '\n':draw' l else draw' l
  draw' [] = "\n"
  draw' ((c, v):r)
    | abs (mod (c - 1) n - v) <= 1 = '#':continue r
    | otherwise                     = '.':continue r

cycleState :: String -> [(Int, Int)]
cycleState = foldl (\ a b -> a ++ processInstruction (fst . last $ a) (snd . last $ a) b) [(1, 1)] .  map instructionFrom . lines

solve1 :: String -> Int
solve2 :: String -> String
solve1 = sum . sumSignalStrengths 20 40 . cycleState
solve2 = drawCRT 40 . init . cycleState
