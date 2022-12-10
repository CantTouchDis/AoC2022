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
  return ()
