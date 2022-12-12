{-# LANGUAGE QuasiQuotes #-}
module Day12
( testInput
, solve1
, solve2
)
where
import Literal
import Data.Char
import Data.List
import qualified Data.Set as Set

testInput :: String
testInput = [litFile|data/test12.txt|]

neighbours :: Int -> Int -> Set.Set (Int, Int)
neighbours y x = Set.fromDistinctAscList [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

findElements :: Int -> Char -> [String] -> [(Int, Int)]
findElements _ _ [] = []
findElements y c (s':r) = zip (repeat y) (elemIndices c s') ++ findElements (y+1) c r

heightMapFrom :: String -> ((Int, Int), (Int, Int), [[Int]])
heightMapFrom s = (head . findElements 0 'S' $ ls, head . findElements 0 'E' $ ls, map (map ord') ls)
  where
    ls = lines s

    ord' 'S' = ord 'a'
    ord' 'E' = ord 'z'
    ord' c = ord c

maxOneUp, maxOneDown :: (Int, Int) -> (Int, Int) -> [[Int]] -> Bool

maxOneUp (fY, fX) (tY, tX) m = elevationTo <= (elevationFrom + 1)
      where
        elevationTo = (m !! tY) !! tX
        elevationFrom = (m !! fY) !! fX

maxOneDown (fY, fX) (tY, tX) m = elevationTo >= (elevationFrom - 1)
      where
        elevationTo = (m !! tY) !! tX
        elevationFrom = (m !! fY) !! fX    

shortestPath2 :: ((Int, Int) -> (Int, Int) -> [[Int]] -> Bool) -> (Int, Int) -> ((Int, Int) -> Bool) -> [[Int]] -> Int
shortestPath2 adjacentP start end m = find' 0 (Set.singleton start) (Set.empty)
  where
    height' = length m
    width'  = length $ head m
    isReachable' f t@(tY, tX) = tY >= 0 && tX >= 0 && tY < height' && tX < width' && adjacentP f t m
    find' :: Int -> Set.Set (Int, Int) -> Set.Set (Int, Int) -> Int
    find' n s v
      | Set.null s = error ""
      | otherwise = 
        let
          waveFront = (flip Set.difference) v . Set.foldl Set.union Set.empty . Set.map (\e -> Set.filter (isReachable' e) . uncurry neighbours $ e) $ s
        in if (0 < (Set.size . Set.filter end) s)
          then n
          else find' (n + 1) waveFront (Set.union s v)


solve1, solve2 :: String -> Int

solve1 = uncurry3 (shortestPath2 maxOneUp) . (\ (a, b, c) -> (a, (\x -> x == b), c)) . heightMapFrom
solve2 = uncurry3 (shortestPath2 maxOneDown) . (\ (_, b, c) -> (b, (\ (cY, cX) -> ord 'a' == (c !! cY) !! cX), c)) . heightMapFrom
