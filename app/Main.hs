module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
--import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day15
import qualified Day17
import qualified Day23

main :: IO ()
main = do
  putStr "Day01:\n"
  putStr $ "solve1: " ++ (show . Day01.solve1 $ Day01.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day01.solve2 $ Day01.testInput) ++ "\n"
  putStr "Day02:\n"
  putStr $ "solve1: " ++ (show . Day02.solve1 $ Day02.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day02.solve2 $ Day02.testInput) ++ "\n"
  putStr "Day03:\n"
  putStr $ "solve1: " ++ (show . Day03.solve1 $ Day03.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day03.solve2 $ Day03.testInput) ++ "\n"
  putStr "Day04:\n"
  putStr $ "solve1: " ++ (show . Day04.solve1 $ Day04.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day04.solve2 $ Day04.testInput) ++ "\n"
  putStr "Day05:\n"
  putStr $ "solve1: " ++ (show . Day05.solve1 $ Day05.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day05.solve2 $ Day05.testInput) ++ "\n"
  putStr "Day06:\n"
  putStr $ "solve1: " ++ (show . Day06.solve1 $ Day06.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day06.solve2 $ Day06.testInput) ++ "\n"
  putStr "Day07:\n"
  putStr $ "solve1: " ++ (show . Day07.solve1 $ Day07.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day07.solve2 $ Day07.testInput) ++ "\n"
  putStr "Day08:\n"
  putStr $ "solve1: " ++ (show . Day08.solve1 $ Day08.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day08.solve2 $ Day08.testInput) ++ "\n"
  putStr "Day09:\n"
  putStr $ "solve1: " ++ (show . Day09.solve1 $ Day09.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day09.solve2 $ Day09.testInput2) ++ "\n"
  putStr "Day10:\n"
  putStr $ "solve1: " ++ (show . Day10.solve1 $ Day10.testInput) ++ "\n"
  putStr $ "solve2: \n" ++ (Day10.solve2 $ Day10.testInput) ++ "\n"
  --putStr "Day11:\n"
  --putStr $ "solve1: " ++ (show . Day11.solve1 $ Day11.testInput) ++ "\n"
  --putStr $ "solve2: " ++ (show . Day11.solve2 $ Day11.testInput) ++ "\n"
  putStr "Day12:\n"
  putStr $ "solve1: " ++ (show . Day12.solve1 $ Day12.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day12.solve2 $ Day12.testInput) ++ "\n"
  putStr "Day13:\n"
  putStr $ "solve1: " ++ (show . Day13.solve1 $ Day13.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day13.solve2 $ Day13.testInput) ++ "\n"
  putStr "Day15:\n"
  putStr $ "solve1: " ++ (show . Day15.solve1 $ Day15.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day15.solve2 $ Day15.testInput) ++ "\n"
  putStr "Day17:\n"
  putStr $ "solve1: " ++ (show . Day17.solve1 $ Day17.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day17.solve2 $ Day17.testInput) ++ "\n"
  putStr "Day23:\n"
  putStr $ "solve1: " ++ (show . Day23.solve1 $ Day23.testInput) ++ "\n"
  putStr $ "solve2: " ++ (show . Day23.solve2 $ Day23.testInput) ++ "\n"
  return ()
