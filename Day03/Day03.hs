module Day03
( testInput
, solve1
, solve2
)
where
import Data.Char

testInput :: String
testInput = "vJrwpWtwJgWrhcsFMMfFFhFp\n\
\jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
\PmmdzqPrVvPwwTWBwg\n\
\wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
\ttgJtRGJQctTZtZT\n\
\CrZsJsPPZsGzwwsLwLmpwMDw"

valueOfChar :: Char -> Int
valueOfChar c
  | ord c >= 65 && ord c < 97 = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1


inBoth :: (Foldable t, Eq a) => [a] -> t a -> [a]
inBoth x y = filter (`elem` y) x

handleGroup :: Eq a => [[a]] -> [[a]]
handleGroup (x:y:z:rest) = [inBoth x $ inBoth y z] ++ handleGroup rest
handleGroup _ = []

solve1, solve2 :: String -> Int
solve1 = sum . map (valueOfChar . head) . map (\ (x, y) -> inBoth x y) . map (\x -> (take (length x `div` 2) x , drop (length x `div` 2) x)) . lines
solve2 = sum . map (valueOfChar . head) . handleGroup . lines