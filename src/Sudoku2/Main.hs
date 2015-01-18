import Data.List
import Data.Function (on)
import Control.Parallel.Strategies

type Pos = (Int, Int) 
type Cell = (Pos, Int)
type Problem = [Cell]

readInt :: Char -> Int
readInt x = read [x]

chunk :: [a] -> [[a]]
chunk [] = [[]]
chunk xs = h : chunk t
  where (h,t) = splitAt 9 xs

showStrProblem :: String -> IO ()
showStrProblem s = putStr $ intercalate "\n" (chunk s) ++ "\n"

showSolution :: Problem -> IO ()
showSolution = putStr . intercalate "\n" . chunk . concatMap (show . snd) . sortBy (compare `on` fst) 

toProblem :: String -> Problem
toProblem = toProblem' 0
  where
    toProblem' 81 _ = []
    toProblem' n (c:cs) 
      | c == '.' = toProblem' (n+1) cs
      | otherwise = ((n `div` 9, n `mod` 9), readInt c) : toProblem' (n+1) cs

-- extractRow, extractCol, extractBlock :: Problem -> Pos -> [Int]
-- extractRow p (x,_) = map snd $ filter (\((x',_), _) -> x' == x) p
-- extractCol p (_,y) = map snd $ filter (\((_,y'), _) -> y' == y) p
-- extractBlock p pos = map snd $ filter (\(pos',_) -> pos' `elem` poss) p
--   where
--     poss = cellnum2Poss . pos2Blocknum $ pos


pos2Blocknum :: Pos -> Int
pos2Blocknum (x,y) = (x `div` 3) * 3 + (y `div` 3)

-- cellnum2Poss :: Int -> [Pos]
-- cellnum2Poss n = [(x,y) 
--                  | let x' = n `div` 3 * 3
--                  , let y' = n `mod` 3 * 3
--                  , x <- [x'..x'+2]
--                  , y <- [y'..y'+2]
--                  ]

candidate :: Problem -> Pos -> [Int]
candidate p pos = [1..9] \\ used p pos

used :: Problem -> Pos -> [Int]
used p pos = [ v
             | (pos', v) <- p
             , any (\f -> f pos == f pos') [fst, snd, pos2Blocknum]
             ]
-- candidate p pos = rowdiff `intersect` coldiff `intersect` celldiff 
--   where 
--     rowdiff = [1..9] \\ extractRow p pos 
--     coldiff = [1..9] \\ extractCol p pos
--     celldiff = [1..9] \\ extractBlock p pos


solve :: Problem -> [Problem]
solve p
  | length p == 81 = return p
  | otherwise = [(pos, v) : p
                | let emptyPoss = [(a,b) | a <- [0..8], b <- [0..8]] \\ map fst p
                      pos = minimumBy (compare `on` length . candidate p) emptyPoss
                , v <- candidate p pos
                ] >>= solve


main :: IO ()
main = do
  contents <- readFile "data/Sudoku/sudoku.txt"
  let solutions = parMap rdeepseq (head . filter (not . null) . solve . toProblem) $ lines contents
  mapM_ showSolution solutions

