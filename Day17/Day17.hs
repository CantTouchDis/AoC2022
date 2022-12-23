{-# LANGUAGE QuasiQuotes #-}

module Day17
( testInput
, solve1
, solve2
)
where

import Literal

import qualified Data.Set as Set
import qualified Data.Map as Map

testInput :: String
testInput = [litFile|data/test17.txt|]

-- shapes :: [Set.Set (Int, Int)]
-- shapes = [
--   Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0)],
--   Set.fromList [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
--   Set.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
--   Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)],
--   Set.fromList [(0, 0), (1, 0), (0, 1), (1, 1)]]

-- simulateN :: Int -> String -> [Set.Set (Int, Int)] -> (Int, Set.Set (Int, Int))
-- simulateN = go' 0 Set.empty
--   where
--     go' :: Int -> Set.Set (Int, Int) -> Int -> String -> [Set.Set (Int, Int)] -> (Int, Set.Set (Int, Int))
--     go' h board 0 _ _ = (h, board)
--     go' h board n w (s:restS) =
--       let
--         (restWind,finalShapePos) = simulateRock w (Set.map (\ (x, y) -> (x + 2, y + h + 3)) s) board
--         newH = Set.foldl (\ mH (_,y) -> max mH (y + 1)) h finalShapePos
--       in
--        go' newH (Set.union board finalShapePos) (n-1) restWind restS

-- showPattern :: (Int, Set.Set (Int, Int)) -> (Int, Set.Set (Int, Int))
-- showPattern a@(_, board) = traceShow pat $ a
--   where
--     p n = Set.toList . Set.map (snd) . Set.filter ((==n) . fst) $ board
--     pat = zipWith (-) (tail $ p 2) (p 2)

-- moves the rock via gusts
updateRock :: Char -> Set.Set (Int, Int) -> Set.Set (Int, Int) -> (Bool, Set.Set (Int, Int))
updateRock '<' s b = (canMoveLeft, if canMoveLeft then leftMovedSet else s)
  where
    canMoveLeft = (Set.null . Set.filter ((==0) . fst) $ s) && (Set.null $ Set.intersection leftMovedSet b)
    leftMovedSet = Set.map (\ (x, y) -> (x - 1, y)) s
updateRock '>' s b = (canMoveRight, if canMoveRight then rightMovedSet else s)
  where
    canMoveRight = (Set.null . Set.filter ((==6) . fst) $ s) && (Set.null $ Set.intersection rightMovedSet b)
    rightMovedSet = Set.map (\ (x, y) -> (x + 1, y)) s
updateRock _ _ _ = undefined

fallRock :: Set.Set (Int, Int) ->  Set.Set (Int, Int) -> (Bool, Set.Set (Int, Int))
fallRock s b = (canFall, if canFall then downMovedSet else s)
  where
    canFall = (Set.null . Set.filter ((==0) . snd) $ s) && (Set.null $ Set.intersection downMovedSet b)
    downMovedSet = Set.map (\ (x, y) -> (x, y - 1)) s

-- Computes the final position of the rock
simulateRock :: String -> Set.Set (Int, Int) -> Set.Set (Int, Int) -> (String, Set.Set (Int, Int))
simulateRock (c:r) s b = let
  (_,next) = updateRock c s b
  (canFall,afterFall) = fallRock next b
  in if canFall
    then simulateRock r afterFall b
    else (r, next)
simulateRock [] _ _ = undefined

data Shape = Hori | Plus | Corn | Vert | Cube deriving (Show, Eq, Ord)

shapesD :: Shape -> Set.Set (Int, Int)
shapesD Hori = Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]
shapesD Plus = Set.fromList [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
shapesD Corn = Set.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
shapesD Vert = Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)]
shapesD Cube = Set.fromList [(0, 0), (1, 0), (0, 1), (1, 1)]

-- does the same as simulateN but checks if a cycle is found
cycleDetectingSimulate :: Int -> String -> Int
cycleDetectingSimulate steps windP = go' 0 0 Set.empty Map.empty (cycle [Hori,Plus,Corn,Vert,Cube]) (cycle windP) steps
  where
    sizeWindP = length windP
    go' :: Int -> Int -> Set.Set (Int, Int) -> Map.Map (String, Shape, Int, Int) (Int, Int) -> [Shape] -> String -> Int -> Int
    go' _ h _ _ _ _ 0 = h
    go' v h board cycleMap (s:restS) windP' remSteps =
      let
        (restWind,finalShapePos) = simulateRock windP' (Set.map (\ (x, y) -> (x + 2, y + h + 3)) . shapesD $ s) board
        newH = Set.foldl (\ mH (_,y) -> max mH (y + 1)) h finalShapePos
        cycleKey = (take sizeWindP windP', s, newH - h, Set.foldl (\m (_,a) -> min m a) 9 finalShapePos)
        lu = Map.lookup cycleKey cycleMap
      in
        case (lu, v) of
          -- only if 5 shapes in a row (all shapes) show a cycle we use it!
          (Just (s',h'), 2) ->
            let
              cycleSteps = (s' - remSteps)
              cycleHeight = (newH - h')
              (d,r) = divMod (remSteps - 1) cycleSteps
              adjustedHeight = {-trace ("Found cycle of " ++ show cycleSteps ++ " steps with heightDelta: " ++ show cycleHeight ++ " adjusting by: " ++ show (d * cycleHeight)) $-} d * cycleHeight
            in
              adjustedHeight + go' 5 newH (Set.union board finalShapePos) Map.empty restS restWind r
          (Just _, _) -> go' (v + 1) newH (Set.union board finalShapePos) (Map.insert cycleKey (remSteps, newH) cycleMap) restS restWind (remSteps-1)
          _                 -> go' 0 newH (Set.union board finalShapePos) (Map.insert cycleKey (remSteps, newH) cycleMap) restS restWind (remSteps-1)
    go' _ _ _ _ [] _ _ = undefined -- Can never happen

solve1, solve2 :: String -> Int
--solve1 s = fst $ simulateN 2022 (cycle s) (cycle shapes)
solve1 = cycleDetectingSimulate 2022
solve2 = cycleDetectingSimulate 1000000000000
