{-# LANGUAGE QuasiQuotes #-}
module Day13
( testInput
, solve1
, solve2
)
where

import Literal
import Data.Char
import Data.List

testInput :: String
testInput = [litFile|data/test13.txt|]

data LS = L [LS] | V Int deriving (Show,Eq)

instance Ord LS where
  compare (V a) (V b)   = compare a b
  compare (L as) (L bs) = cmpL' as bs
    where
      cmpL' [] [] = EQ
      cmpL' [] _  = LT
      cmpL' _ []  = GT
      cmpL' (a:ra) (b:rb)
        | a == b    = cmpL' ra rb
        | otherwise = compare a b
  compare a b = compare (mkList' a) (mkList' b)
    where
      mkList' (V a') = L [V a']
      mkList' a'     = a'

lsFrom :: String -> (LS, String)
lsFrom ('[':r) = listFrom' [] r
  where
  listFrom' c (']':r') = (L c, r')
  listFrom' c (',':r') = listFrom' c r'
  listFrom' c s        = 
    let
      (e, r') = lsFrom s
    in
      listFrom' (c ++ [e]) r'
lsFrom s = (V . read . takeWhile isDigit $ s, dropWhile isDigit s)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n l = first : (splitEvery n rest)
  where
    (first, rest) = splitAt n l

dividers :: [LS]
dividers = [(L [L [V 2]]), (L [L [V 6]])]

solve1, solve2 :: String -> Int
solve1 = sum . map fst . filter ((==LT) . snd) . zip [1..] . map (\ [a, b] -> compare a b) . splitEvery 2 . map lsFrom . filter (not . null) . lines
solve2 = (\ [Just a, Just b] -> (a + 1) * (b + 1)) . ((<*>) [elemIndex (head dividers), elemIndex (last dividers)] . pure) . sort . ((++) dividers) . map (fst . lsFrom) . filter (not . null) . lines