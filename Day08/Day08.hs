module Day08
( testInput
, solve1
, solve2
)
where
import Data.List
import Data.Char

testInput :: String
testInput = "30373\n\
\25512\n\
\65332\n\
\33549\n\
\35390"

lineFrom :: String -> [Int]
lineFrom (a:rest) = (ord a - ord '0'):(lineFrom rest)
lineFrom [] = []

markInRow :: Int -> [Int] -> [Bool]
markInRow m (e:r)
  | e > m       = [True] ++ markInRow e r
  | otherwise   = [False] ++ markInRow m r
markInRow _ []  = []

markVisible :: [[Int]] -> [[Bool]]
markVisible inp = combine (combine fromLeft fromRight) (combine fromTop fromBottom)
  where
  combine = zipWith (\ a' b' -> zipWith (||) a' b')
  fromLeft = map (markInRow (-1)) inp
  fromRight = map (reverse . (markInRow (-1)) . reverse) inp
  fromTop = transpose . map (markInRow (-1)) . transpose $ inp
  fromBottom = transpose . map (reverse . (markInRow (-1)) . reverse) . transpose $ inp

scenicScoreLine :: [Int] -> [Int]
scenicScoreLine = lineScores' []
  where
  lineScores' leftR (e:rest) = [length (takeSmaller e leftR) * length (takeSmaller e rest)] ++ lineScores' (e:leftR) rest
  lineScores' _ [] = []
  takeSmaller e l = takeWhile (<e) l ++ (take 1 . dropWhile (<e) $ l)

scenicScore :: [[Int]] -> [[Int]]
scenicScore inp = zipWith (\ a b -> zipWith (*) a b) lr tb
  where
  lr = map (scenicScoreLine) inp
  tb = transpose . map (scenicScoreLine) . transpose $ inp

solve1, solve2 :: String -> Int
solve1 = sum . map (length . filter (==True)) . markVisible . map lineFrom . lines
solve2 = maximum . map maximum . scenicScore . map lineFrom . lines
