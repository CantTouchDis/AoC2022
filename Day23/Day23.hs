{-# LANGUAGE QuasiQuotes #-}

module Day23
( testInput
, solve1
, solve2
)
where

import Literal
import qualified Data.Set as Set
import qualified Data.Map as Map

testInput :: String
testInput = [litFile|data/test23.txt|]


elfCoordsFrom :: String -> [(Int, Int)]
elfCoordsFrom = go' 0 . lines
  where
    elvesInRow :: Int -> String -> [Int]
    elvesInRow _ [] = []
    elvesInRow x ('#':r) = x:elvesInRow (x+1) r
    elvesInRow x ('.':r) = elvesInRow (x+1) r
    elvesInRow _ _ = undefined
    go' :: Int -> [String] -> [(Int, Int)]
    go' _ [] = []
    go' r (s:rs) = [(r, x) | x <- elvesInRow 0 s] ++ go' (r + 1) rs

data Direction = North | South | West | East deriving (Show)


canMoveInDirection :: (Int, Int) -> Direction -> Set.Set (Int, Int) -> Bool
canMoveInDirection (pY,pX) North = Set.null . Set.intersection (Set.fromAscList [(pY - 1, pX - 1), (pY - 1, pX), (pY - 1, pX + 1)])
canMoveInDirection (pY,pX) South = Set.null . Set.intersection (Set.fromAscList [(pY + 1, pX - 1), (pY + 1, pX), (pY + 1, pX + 1)])
canMoveInDirection (pY,pX) West = Set.null . Set.intersection (Set.fromAscList [(pY - 1, pX - 1), (pY, pX - 1), (pY + 1, pX - 1)])
canMoveInDirection (pY,pX) East = Set.null . Set.intersection (Set.fromAscList [(pY - 1, pX + 1), (pY, pX + 1), (pY + 1, pX + 1)])

posInDirection :: (Int, Int) -> Direction -> (Int, Int)
posInDirection (pY,pX) North = (pY - 1,pX)
posInDirection (pY,pX) South = (pY + 1,pX)
posInDirection (pY,pX) West  = (pY,pX - 1)
posInDirection (pY,pX) East  = (pY,pX + 1)

standStill :: (Int, Int) -> Set.Set (Int, Int) -> Bool
standStill (pY,pX) = Set.null . Set.intersection (Set.fromAscList [(pY - 1, pX - 1), (pY - 1, pX), (pY - 1, pX + 1), (pY, pX - 1), (pY, pX + 1), (pY + 1, pX - 1), (pY + 1, pX), (pY + 1, pX + 1)])

solve :: [Direction] -> Set.Set (Int, Int) -> Int -> Set.Set (Int, Int)
solve = go
  where
    go :: [Direction] -> Set.Set (Int, Int) -> Int -> Set.Set (Int, Int)
    go _ state 0 = state
    go dirs@(_:rD) state n = go rD newState (n-1)
      where
        newState :: Set.Set (Int, Int)
        newState = Set.union (Map.keysSet newPositions) (Set.fromList $ Map.foldl (++) [] collisions)
        newPositions, collisions :: Map.Map (Int,Int) [(Int, Int)]
        (newPositions,collisions) = Map.partition ((==1) . length) . Map.fromListWith (++) $ map (flip (step dirs) $ state) $ Set.toList state

        step :: [Direction] -> (Int, Int) -> Set.Set (Int, Int) -> ((Int, Int), [(Int, Int)])
        step dirs' p board = head ((if standStill p board then [(p, [p])] else []) ++ [(posInDirection p d, [p]) | d <- take 4 dirs', canMoveInDirection p d board] ++ [(p, [p])])
    go [] _ _ = undefined

solveN :: [Direction] -> Set.Set (Int, Int) -> Int
solveN = go 0
  where
    go :: Int -> [Direction] -> Set.Set (Int, Int) -> Int
    go r dirs@(_:rD) state
      | Map.null . Map.filterWithKey (\ a b -> [a] /= b) $ newPositions = r + 1
      | otherwise = go (r+1) rD newState
      where
        newState :: Set.Set (Int, Int)
        newState = Set.union (Map.keysSet newPositions) (Set.fromList $ Map.foldl (++) [] collisions)
        newPositions, collisions :: Map.Map (Int,Int) [(Int, Int)]
        (newPositions,collisions) = Map.partition ((==1) . length) . Map.fromListWith (++) $ map (flip (step dirs) $ state) $ Set.toList state

        step :: [Direction] -> (Int, Int) -> Set.Set (Int, Int) -> ((Int, Int), [(Int, Int)])
        step dirs' p board = head ((if standStill p board then [(p, [p])] else []) ++ [(posInDirection p d, [p]) | d <- take 4 dirs', canMoveInDirection p d board] ++ [(p, [p])])
    go _ [] _ = undefined

coveredArea :: Set.Set (Int, Int) -> Int
coveredArea s = area - Set.size s
  where
    width = uncurry (-) . Set.foldl (\(a,b) (_,d) -> (max a d, min b d)) (minBound :: Int, maxBound :: Int) $ s
    area = ((fst $ Set.findMax s) - (fst $ Set.findMin s) + 1) * (width + 1)

solve1, solve2 :: String -> Int
solve1 = coveredArea . (flip $ solve (cycle [North, South, West, East])) 10 . Set.fromAscList . elfCoordsFrom
solve2 = solveN (cycle [North, South, West, East]) . Set.fromAscList . elfCoordsFrom
