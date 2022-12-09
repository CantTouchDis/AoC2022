{-# LANGUAGE ViewPatterns #-}
module Day07
( testInput
, solve1
, solve2
)
where
import Data.List

testInput :: String
testInput = "$ cd /\n\
\$ ls\n\
\dir a\n\
\14848514 b.txt\n\
\8504156 c.dat\n\
\dir d\n\
\$ cd a\n\
\$ ls\n\
\dir e\n\
\29116 f\n\
\2557 g\n\
\62596 h.lst\n\
\$ cd e\n\
\$ ls\n\
\584 i\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd d\n\
\$ ls\n\
\4060174 j\n\
\8033020 d.log\n\
\5626152 d.ext\n\
\7214296 k"

data FS = DIR String [FS] | FILE String Int deriving (Show)

lsFrom :: String -> FS
lsFrom s
  | "dir" == take 3 s = DIR (drop 4 s) []
  | otherwise         = FILE name (read size)
    where
    [size, name] = words s

data CMD = ROOT | UP | DOWN String | LIST [String] deriving (Show) 

cmdFrom :: [String] -> (CMD, [String])
cmdFrom s =
  let
    line = head $ s
    output = takeWhile ((/='$') . head) . tail $ s
    rest = dropWhile ((/='$') . head) . tail $ s
  in case line of
    "$ cd /"                        -> (ROOT, rest)
    "$ cd .."                       -> (UP, rest)
    (stripPrefix "$ cd " -> Just r) -> (DOWN r, rest)
    _                               -> (LIST output, rest)
    
isFile :: FS -> Bool
isFile (FILE _ _) = True
isFile _          = False


mergeInto :: FS -> [FS] -> [FS]
mergeInto e (f@(FILE _ _):r) = f:(mergeInto e r)
mergeInto d1@(DIR n a) (d2@(DIR n1 b):r)
  | n == n1 = (DIR n1 (b ++ a)):r
  | otherwise = d2:(mergeInto d1 r)
mergeInto e [] = [e]
mergeInto _ _ = undefined

mergeIntoD :: FS -> FS -> FS
mergeIntoD e (DIR n fs) = DIR n (mergeInto e fs)
mergeIntoD _ _ = undefined

fsFrom :: [String] -> FS
fsFrom = lsFrom' [DIR "/" []]
  where
  popFromStack (d:d2:rest) = (mergeIntoD d d2):rest
  popFromStack _ = undefined
  files = filter (isFile) . map lsFrom
  lsFrom' [e] [] = e
  lsFrom' (a:b:r) [] = lsFrom' ((mergeIntoD a b):r) []
  lsFrom' pathStack@(d:r) conOut =
    let
      (cmd, rest) = cmdFrom conOut
    in case cmd of
      ROOT -> if length pathStack == 1 then lsFrom' pathStack rest else lsFrom' (popFromStack pathStack) conOut
      LIST ls -> lsFrom' ((foldl (\ a b -> mergeIntoD b a) d . files $ ls):r) rest
      DOWN n' -> lsFrom' ((DIR n' []):pathStack) rest
      UP -> lsFrom' ((mergeIntoD d (head r)):(tail r)) rest
  lsFrom' _ _ = undefined

totalSize :: FS -> Int
totalSize (FILE _ size) = size
totalSize (DIR _ fs) = sum . map (totalSize) $ fs


solve1 :: String -> Int
solve1 s = solve1' fs
  where
  fs = fsFrom . drop 1 . lines $ s
  solve1' d@(DIR _ fs') = (if s' <= 100000 then s' else 0) + (sum . map solve1') fs'
    where
    s' = totalSize d
  solve1' _ = 0

solve2 :: String -> Int
solve2 s = head . dropWhile (<requiredDelete) . sort . solve2' $ fs
  where
  fs = fsFrom . drop 1 . lines $ s
  requiredDelete = (30000000 - (70000000 - totalSize fs))
  solve2' :: FS -> [Int]
  solve2' (FILE _ _) = []
  solve2' d@(DIR _ fs') = ((totalSize d):(foldl (++) [] . map solve2' $ fs'))