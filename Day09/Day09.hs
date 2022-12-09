module Day09
( testInput
, testInput2
, solve1
, solve2
)
where
import Data.List

testInput :: String
testInput = "R 4\n\
\U 4\n\
\L 3\n\
\D 1\n\
\R 4\n\
\D 1\n\
\L 5\n\
\R 2"

testInput2 :: String
testInput2 = "R 5\n\
\U 8\n\
\L 8\n\
\D 3\n\
\R 17\n\
\D 10\n\
\L 25\n\
\U 20"

type Rope = [(Int, Int)]

data Dir = U | D | L | R deriving (Show, Read)

dragBehind :: Rope -> Rope
dragBehind r@(h@(hY, hX):(tY, tX):[])
  | abs dY > 1 || abs dX > 1 = h:(tY + signum dY, tX + signum dX):[]
  | otherwise = r
  where
    dY = hY - tY
    dX = hX - tX
dragBehind (h:t:r) = 
  let
    [_, t1] = dragBehind (h:t:[])
  in
    h:(dragBehind (t1:r))
dragBehind _ = undefined

updateHead :: Dir -> (Int, Int) -> (Int, Int)
updateHead U (hY, hX) = (hY + 1, hX)
updateHead D (hY, hX) = (hY - 1, hX)
updateHead L (hY, hX) = (hY, hX - 1)
updateHead R (hY, hX) = (hY, hX + 1)

moveRopeFrom :: Int -> [String] -> [Rope]
moveRopeFrom n = moveRopeFrom' [take n . repeat $ (0,0)]
  where
  moveRopeFrom' :: [Rope] -> [String] -> [Rope]
  moveRopeFrom' a [] = a
  moveRopeFrom' (c:r) (l:ls) =
      moveRopeFrom' (pos ++ r) ls
    where
    [dirS, countS] = words l
    (d, cnt) = (read dirS, read countS)
    pos :: [Rope]
    pos = reverse . take (cnt+1) $ iterate (\ (h:r') -> dragBehind ((updateHead d h):r')) c
  moveRopeFrom' _ _ = undefined

solve1, solve2 :: String -> Int
solve1 = length . nub . sort . map last . moveRopeFrom 2 . lines
solve2 = length . nub . sort . map last . moveRopeFrom 10 . lines
