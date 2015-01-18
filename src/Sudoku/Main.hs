import Data.List
import Control.Parallel.Strategies

type Problem = [[Int]]
type Candidate = [[[Int]]]
newtype Pos = Pos (Int, Int) deriving (Eq, Show)

toPos :: (Int, Int) -> Pos
toPos (a, b)
  | 0 <= a && a <= 8 && 0 <= b && b <= 8 = Pos (a, b)
  | otherwise = error "invalid"

fromPos :: Pos -> (Int, Int)
fromPos (Pos (a, b)) = (a, b)
               
instance Enum Pos where
  succ (Pos (a, b))
   | b == 8 = Pos (a+1, 0)
   | otherwise = Pos (a, b+1)
  pred (Pos (a, b))
   | b == 0 = Pos (a-1, 8)
   | otherwise = Pos (a, b-1)
  toEnum n = Pos (n `div` 9, n `mod` 9)
  fromEnum (Pos (a, b)) = a * 9 + b

instance Bounded Pos where
  minBound = Pos (0,0)
  maxBound = Pos (8,8)
  
cellCenters :: [Pos]
cellCenters = map toPos [(1,1), (1,4), (1,7)
                        ,(4,1), (4,4), (4,7)
                        ,(7,1), (7,4), (7,7)
                        ]

data ProblemState = NotSolved | Wrong | Solved deriving (Eq, Show)

-- difficult
problem :: Problem
problem = [[3,0,0,0,0,2,0,0,0]
          ,[0,4,0,8,0,5,0,3,0]
          ,[2,9,0,0,0,0,0,0,8]
          ,[4,0,0,1,7,0,0,0,5]
          ,[0,0,6,0,5,8,3,0,0]
          ,[0,0,0,2,4,6,0,0,0]
          ,[0,0,0,5,0,0,6,0,0]
          ,[0,8,0,6,0,4,0,5,0]
          ,[0,0,2,0,0,0,9,8,0]
          ]

-- easy
problemEasy :: Problem
problemEasy = [[0,9,0,0,0,3,0,2,0]
              ,[7,0,2,6,0,0,5,4,0]
              ,[0,0,0,0,0,8,0,0,7]
              ,[1,4,0,8,0,0,0,0,3]
              ,[0,5,9,7,3,0,0,0,1]
              ,[8,6,0,1,0,9,7,5,2]
              ,[9,2,1,3,0,4,6,0,5]
              ,[0,8,0,9,0,7,2,1,4]
              ,[4,0,6,0,0,5,8,3,0]
              ]
         
lookupPos :: [[a]] -> Pos -> a
lookupPos p (Pos (x,y)) = (p !! x) !! y

chunk :: [a] -> [[a]]
chunk [] = [[]]
chunk xs = h : chunk t
  where (h,t) = splitAt 9 xs

pos2Cellnum :: Pos -> Int
pos2Cellnum (Pos (x,y)) = (x `div` 3) * 3 + (y `div` 3)
  
cellnum2Pos :: Int -> Pos
cellnum2Pos n = toPos ((*) 3 $ div n 3, (*) 3 $ mod n 3)

extractRow, extractCol, extractCell :: [[a]] -> Pos -> [a]
extractRow p (Pos (x,_)) = (!!x) p
extractCol p (Pos (_,y)) = map (!!y) p
extractCell p pos = concatMap (splice3 y) . splice3 x $ p
  where
    Pos (x, y) = cellnum2Pos . pos2Cellnum $ pos
    splice3 :: Int -> [a] -> [a]
    splice3 n = drop n . take (n+3)

p2c :: Problem -> Candidate
p2c problem = filter (not . null) $ chunk $ map (candidateInPos problem) ([minBound..maxBound]::[Pos])


c2p :: Candidate -> Problem
c2p = map (map convert)
  where
    convert :: [Int] -> Int
    convert ns
      | length ns == 1 = head ns
      | otherwise = 0

decide :: Candidate -> Candidate
decide = transposeCell . map decideRow . transposeCell
         . transpose . map decideRow . transpose 
         . map decideRow

decideRow :: (Eq a, Ord a) => [[a]] -> [[a]]
decideRow nss = map buildRow nss
  where
    decidedList = concat . filter (\es -> length es == 1) . group . sort . concat $ nss
    buildRow ns = if length ns' == 1 then ns' else ns
      where
        ns' = ns `intersect` decidedList

transposeCell :: [[a]] -> [[a]]
transposeCell candidate = map (extractCell candidate) cellCenters

candidateInPos :: Problem -> Pos -> [Int]
candidateInPos problem pos
  | e == 0 = rowdiff `intersect` coldiff `intersect` celldiff 
  | otherwise = [e]
  where 
    e = lookupPos problem pos
    rowdiff = [1..9] \\ extractRow problem pos 
    coldiff = [1..9] \\ extractCol problem pos
    celldiff = [1..9] \\ extractCell problem pos

replace :: [[a]] -> Int -> [a] -> [[a]]
replace p n xs 
  | 0 <= n || n <= 8 = take n p ++ [xs] ++ drop (n+1) p
  | otherwise = p

validate :: Problem -> ProblemState
validate p = if elem 0 $ concat p 
               then NotSolved
               else case all validate' $ p ++ transpose p ++ transposeCell p of
                      True -> Solved
                      False -> Wrong
  where
    validate' ns = null $ [1..9] \\ ns

step1 :: Problem -> Problem -> Problem
step1 problem prev 
  | problem == prev = problem
  | otherwise = let c = p2c problem
                    c' = decide c
                    p = c2p c'
                in step1 p problem

step2 :: Problem -> Int -> Problem
step2 p n 
  | validate p == Solved = p
  | validate p == Wrong = [[]]
  | otherwise = let ps' = map (replace p n) $ sequence $ (p2c p) !! n
                in  filter (not . null) $ concatMap (\p' -> step2 (step1 p' [[]]) (n+1)) ps'

solve :: Problem -> Problem
solve p = flip step2 0 $ step1 p [[]]

translate :: Char -> Char
translate '.' = '0'
translate x = x

readInt :: Char -> Int
readInt x = read [x]

main :: IO ()
main = do
  contents <- readFile "data/Sudoku/sudoku.txt" 
  let problems = map (filter (not . null) . chunk . map (readInt . translate)) $ lines contents
  -- print $ map (\p -> step1 p [[]]) problems
  print $ parMap rseq solve problems

