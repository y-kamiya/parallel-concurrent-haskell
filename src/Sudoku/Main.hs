import Data.List
import Data.Tuple
import Control.Monad.State

-- data Cell = [Int] | ((Cell,Cell,Cell),(Cell,Cell,Cell),(Cell,Cell,Cell)) deriving (Eq, Show)

type Problem = [[Int]]
type Candidate = [[[Int]]]
-- type Pos = (Int, Int)
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
-- problem :: Problem
-- problem = [[0,9,0,0,0,3,0,2,0]
--          , [7,0,2,6,0,0,5,4,0]
--          , [0,0,0,0,0,8,0,0,7]
--          , [1,4,0,8,0,0,0,0,3]
--          , [0,5,9,7,3,0,0,0,1]
--          , [8,6,0,1,0,9,7,5,2]
--          , [9,2,1,3,0,4,6,0,5]
--          , [0,8,0,9,0,7,2,1,4]
--          , [4,0,6,0,0,5,8,3,0]
--          ]
         
lookupP :: Problem -> Pos -> Int
lookupP pro (Pos (x,y)) = (pro !! x) !! y

chunk :: [a] -> [[a]]
chunk [] = [[]]
chunk xs = h : chunk t
  where (h,t) = splitAt 9 xs

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
decide = transpose . map decideRow . transpose . map decideRow
-- decide = execState decideAction

-- decideAction :: State Candidate Int
-- decideAction = do
--   candidate <- get
--   let newCandidate = map decideRow candidate
--   put newCandidate
--   return 1

-- transposeCell :: Candidate -> Candidate
-- transposeCell candidate = map (extractCell candidate) cellCenters

decideRow :: [[Int]] -> [[Int]]
decideRow nss = map buildRow nss
  where
    decidedList = concat . filter (\es -> length es == 1) . group . sort . concat $ nss

    buildRow :: [Int] -> [Int]
    buildRow ns = list
      where
        ns' = ns `intersect` decidedList
        list = if length ns' == 1
                 then ns'
                 else ns


candidateInPos :: Problem -> Pos -> [Int]
candidateInPos problem pos
  | e == 0 = rowdiff `intersect` coldiff `intersect` celldiff 
  | otherwise = [e]
  where 
    e = lookupP problem pos
    rowdiff = [1..9] \\ extractRow problem pos 
    coldiff = [1..9] \\ extractCol problem pos
    celldiff = [1..9] \\ extractCell problem pos


extractRow, extractCol, extractCell :: Problem -> Pos -> [Int]
extractRow p (Pos (x,_)) = (!!x) p
extractCol p (Pos (_,y)) = map (!!y) p
extractCell p pos = concatMap (splice3 y) . splice3 x $ p
  where
    Pos (x, y) = cell2Pos . pos2Cell $ pos
    splice3 :: Int -> [a] -> [a]
    splice3 n = drop n . take (n+3)

pos2Cell :: Pos -> Int
pos2Cell (Pos (x,y)) = (x `div` 3) * 3 + (y `div` 3)
  
cell2Pos :: Int -> Pos
cell2Pos n = toPos ((*) 3 $ div n 3, (*) 3 $ mod n 3)


main :: IO ()
main = do
  print $ loop problem [[]]
  where
    loop problem prev 
      | problem == prev = problem
      | otherwise = let c = p2c problem
                        c' = decide c
                        p = c2p c'
                    in loop p problem
