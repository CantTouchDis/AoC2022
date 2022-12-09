module Day01
( testInput
, solve1
, solve2
)
where
import Data.List

testInput :: String
testInput = "1000\n\
\2000\n\
\3000\n\
\\n\
\4000\n\
\\n\
\5000\n\
\6000\n\
\\n\
\7000\n\
\8000\n\
\9000\n\
\\n\
\10000"

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f l = splitOn' [] [] l
  where
  splitOn' res cG [] = res ++ [cG]
  splitOn' res cG (x:xs)
    | f x       = splitOn' (res ++ [cG]) [] xs
    | otherwise = splitOn' res (cG ++ [x]) xs

solve1 :: String -> Int
solve1 = head . reverse . sort . map (sum . map read) . splitOn (=="") . lines 

solve2 :: String -> Int
solve2 = sum . take 3 . reverse . sort . map (sum . map read) . splitOn (=="") . lines