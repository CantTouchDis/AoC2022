{-# LANGUAGE QuasiQuotes #-}

module Day15
( testInput
, solve1
, solve2
)
where

import Literal
import Data.List
import qualified Data.Set as Set

testInput :: String
testInput = [litFile|data/test15.txt|]

type Sensor = (Int, Int, Int, Int, Int)

spheresFrom :: String -> [Sensor]
spheresFrom = map (sphere' . words) . lines
  where
    sphere' :: [String] -> Sensor
    sphere' w = (cX, cY, bX, bY, abs (cX - bX) + abs (cY - bY))
      where
      cX = read . init . drop 2 $ w !! 2
      cY = read . init . drop 2 $ w !! 3
      bX = read . init . drop 2 $ w !! 8
      bY = read . drop 2 $ w !! 9

dimensions :: [Sensor] -> (Int, Int, Int, Int)
dimensions = foldl (\ (minX, minY, maxX, maxY) (x, y, _, _, r) -> (min minX $ x - r, max minY $ y - r, max maxX $ x + r, max maxY $ y + r)) (0, 0, 0, 0)

possibleBeacon :: (Int, Int) -> [Sensor] -> Bool
possibleBeacon (x, y) = not . any (beaconPossible)
  where
    beaconPossible (xS, yS, _, _, rS) = abs (xS - x) + abs (yS - y) <= rS

beaconsInLine :: Int -> [Sensor] -> Int
beaconsInLine y = length . nub . sort . map (\ (_,_,x,_,_) -> x) . filter (\ (_,_,_,bY,_) -> bY == y)

impossibleInLine :: Int -> [Sensor] -> Int
impossibleInLine y s = length . filter (==False) $ map (impossible') [lX..hX]
  where
    impossible' x = possibleBeacon (x, y) s
    (lX, _, hX, _) = dimensions s


inValidRange :: Int -> (Int, Int) -> Bool
inValidRange n (x, y) = 0 <= x && x <= n && 0 <= y && y <= n

circleAround :: ((Int, Int) -> Bool) -> (Int, Int) -> Int -> [(Int, Int)]
circleAround f (cX, cY) r = topRight ++ topLeft ++ bottomRight ++ bottomLeft
  where
    topRight    = [(cX + p, cY - r + p) | p <- [0..r], f (cX + p, cY - r + p)]
    topLeft     = [(cX - p, cY - r + p) | p <- [0..r], f (cX - p, cY - r + p)]
    bottomRight = [(cX + p, cY + r - p) | p <- [0..r], f (cX + p, cY + r - p)]
    bottomLeft  = [(cX - p, cY + r - p) | p <- [0..r], f (cX - p, cY + r - p)]

findDistress :: Int -> [Sensor] -> (Int, Int)
findDistress n s = Set.elemAt 0 positions
  where
    positions = foldl1 Set.union . map (Set.fromList . (\ (cX, cY, _, _, r) -> filter (flip possibleBeacon s) $ circleAround (inValidRange n) (cX, cY) (r + 1))) $ s

tuningFrequency :: (Int, Int) -> Int
tuningFrequency (x, y) = x * 4000000 + y

sol1Row :: Int
sol1Row = 10 --2000000

sol2Bounds :: Int
sol2Bounds = 20 -- 4000000

solve1 :: String -> Int
solve1 =  sum . ((<*>) [impossibleInLine sol1Row, (*(-1)) . beaconsInLine sol1Row] . pure) . spheresFrom

solve2 :: String -> Int
solve2 = tuningFrequency . findDistress sol2Bounds . spheresFrom

