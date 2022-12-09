module Day04
( testInput
, solve1
, solve2
)
where
import Data.Ix
import qualified Data.Set as Set

testInput :: String
testInput = "2-4,6-8\n\
\2-3,4-5\n\
\5-7,7-9\n\
\2-8,3-7\n\
\6-6,4-6\n\
\2-6,4-8"

assignmentFrom :: String -> ([Int], [Int])
assignmentFrom s =
  let
    (a, _:as) = span (/='-') s
    (b, _:bs) = span (/=',') as
    (c, _:cs) = span (/='-') bs
  in
    (range (read a, read b), range (read c, read cs))

makeSets ::
  (Ord a1, Ord a2) => ([a1], [a2]) -> (Set.Set a1, Set.Set a2)
makeSets a = ((Set.fromList . fst) a , (Set.fromList . snd) a)

contained :: Ord a => (Set.Set a, Set.Set a) -> Bool
contained (a, b) = Set.isSubsetOf a b || Set.isSubsetOf b a

solve1, solve2 :: String -> Int
solve1 = length . filter (==True) . map (contained) . map (makeSets . assignmentFrom) . lines
solve2 = length . filter (/=True) . map (\ (a, b) -> Set.null $ Set.intersection a b) . map (makeSets . assignmentFrom) . lines