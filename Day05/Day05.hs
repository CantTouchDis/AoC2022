module Day05
( testInput
, solve1
, solve2
)
where

import Data.List

testInput :: String
testInput = "    [D]    \n\
\[N] [C]    \n\
\[Z] [M] [P]\n\
\ 1   2   3 \n\
\\n\
\move 1 from 2 to 1\n\
\move 3 from 1 to 3\n\
\move 2 from 2 to 1\n\
\move 1 from 1 to 2"

boardFrom :: [String] -> [[Char]]
boardFrom [] = []
boardFrom (s:r) = ((fromLine' s):(boardFrom r))
  where
  fromLine' :: String -> [Char]
  fromLine' [] = []
  fromLine' (_:c:_:r') = c:(fromLine' (drop 1 r'))
  fromLine' _ = undefined

stepFrom :: String -> (Int, Int, Int)
stepFrom s =
  let
    [_,c,_,f,_,t] = words s
  in (read c, read f - 1, read t - 1)

inputFrom :: String -> ([[Char]], [(Int, Int, Int)])
inputFrom s =
  let
    ls = lines s
    (boardS, steps) = (init . takeWhile (/="") $ ls, drop 1 . dropWhile (/="") $ ls)
  in (map (filter (/=' ')) . transpose . boardFrom $ boardS, map stepFrom steps)


moveCrate :: ([Char] -> [Char]) -> [[Char]] -> (Int, Int, Int) -> [[Char]]
moveCrate fun b (c, f, t) = 
  let
    crates = take c (b !! f)
    remaining = drop c (b !! f)
    newStack = (fun crates) ++ (b !! t)
    change :: [[Char]] -> [Char] -> Int -> [[Char]]
    change [] _ _ = undefined
    change (_:rest) b' 0 = b':rest
    change (a:rest) b' n = a:change rest b' (n-1)
  in
    change (change b remaining f) newStack t


crateMover9000 :: [[Char]] -> (Int, Int, Int) -> [[Char]]
crateMover9000 = moveCrate reverse

crateMover9001 :: [[Char]] -> (Int, Int, Int) -> [[Char]]
crateMover9001 = moveCrate id


solve1, solve2 :: String -> String
solve1 = map head . uncurry (foldl crateMover9000) . inputFrom
solve2 = map head . uncurry (foldl crateMover9001) . inputFrom