-- ghci str8ts
-- table (chunksOf 6 (solveIt str8tsBoard gapsBoard))

module Str8ts where
import Data.List ( nub, transpose )
import Data.List.Split
    ( chunksOf, condense, dropBlanks, split, whenElt )

--https://www.janko.at/Raetsel/Straights/105.a.htm
str8tsBoard_6x6 :: [Int]
str8tsBoard_6x6 = [-1, 0, 0, 0, -1, 4,
                   -1, 0, 0, 0, 0, 1,
                    0, 0, 3, -1, 0, 0,
                    5, 0, -1, -1, 0, 0,
                    6, 0, 4, 0, 0, -1,
                   -1, 1, 0, 0, 0, -1]

gapsBoard_6x6 :: [Int]
gapsBoard_6x6 = [0, 1, 1, 1, 0, 0,
                 0, 1, 1, 1, 1, 1,
                 1, 1, 0, 0, 1, 1,
                 1, 1, 0, 0, 1, 1,
                 1, 1, 1, 1, 1, 0,
                 0, 0, 1, 1, 1, 0]
----------------------------------------------------------------
--https://www.janko.at/Raetsel/Straights/100.a.htm
str8tsBoard_9x9 :: [Int]
str8tsBoard_9x9 = [0, 0, -1, -1, 0, 0, -1, 0, 0,
                   6, 0, 0, 0, 0, -1, 0, 3, 0,
                   -1, 0, 0, 0, 0, 4, 0, 0, 0,
                   0, 0, 4, -1, 0, 0, 0, 0, -1,
                   0, 0, 0, 0, 0, 0, 0, 0, 0,
                   1, 0, 0, 0, 0, -1, 7, 9, 0,
                   0, 0, 0, 8, 0, 0, 0, 0, -1,
                   0, 0, 0, -1, 9, 0, 0, 5, 0,
                   0, 0, 9, 0, 0, 5, -1, 0, 0]

gapsBoard_9x9 :: [Int]
gapsBoard_9x9 = [1, 1, 0, 0, 1, 1, 0, 1, 1,
                 1, 1, 1, 1, 1, 0, 1, 1, 1,
                 0, 1, 1, 1, 1, 0, 1, 1, 1,
                 1, 1, 0, 0, 1, 1, 1, 1, 0,
                 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 0, 1, 1, 1, 1, 0, 0, 1, 1,
                 1, 1, 1, 0, 1, 1, 1, 1, 0,
                 1, 1, 1, 0, 1, 1, 1, 1, 1,
                 1, 1, 0, 1, 1, 0, 0, 1, 1]

boardSize :: [Int] -> Int
boardSize [] = 0
boardSize s = length(s)

lineWidth :: [Int] -> Int
lineWidth [] = 0
lineWidth s = intSquareRoot (boardSize s)

intSquareRoot :: Int -> Int
intSquareRoot n = try n where
  try i   | i*i > n   = try (i - 1) 
          | i*i <= n  = i

-- converts an index i into an x and y co-ordinate
indexToCoord :: Int -> Int -> (Int, Int)
indexToCoord i l= (calcX i l, calcY i l)
  where calcX i l  = i - l * (i `div` l)
        calcY i l  = i `div` l

-- Takes an x and y co-ordinate and converts it into an index
coordToIndex :: (Int, Int) -> Int -> Int
coordToIndex (x, y) l= x + y * l

-- Retrieves the vertical column of values from the board (s) at the index (p)
columnAt :: Int -> [Int] -> [Int]
columnAt p s = helperColumnAt (indexToCoord p (lineWidth s)) s
  where helperColumnAt (x, _) s = map (\y -> s !! coordToIndex (x, y) (lineWidth s)) [0..((lineWidth s)-1)] 

-- Retrieves the horizontal row of values from the board (s) at the index (p)
rowAt :: Int -> [Int] -> [Int]
rowAt p s = helperRowAt (indexToCoord p (lineWidth s)) s
  where helperRowAt (_, y) s = map (\x -> s !! coordToIndex (x, y) (lineWidth s)) [0..((lineWidth s)-1)]

-- Removes the elements in the second list from the first list
remove :: [Int] -> [Int] -> [Int]
remove [] _       = []
remove xs []      = xs
remove xs (y:ys)  = remove (removeAll y xs) ys

-- Remove all occurences of a value in a list
removeAll :: Int -> [Int] -> [Int]
removeAll _ []     = []
removeAll y (x:xs) | x == y    = removeAll y xs
                   | otherwise = x : removeAll y xs

-- The list of solutions at the index p of board s
solutionsAt :: Int -> [Int] -> [Int]
solutionsAt p s | p > length s  = []
                | ((s !! p) == 0) = [1..(lineWidth s)] `remove` (columnAt p s ++ rowAt p s)
                | otherwise     = [s !! p]

noGaps :: (Enum a, Ord a) => [a] -> Bool
noGaps [] = True
noGaps xs = all (`elem` xs) [minimum xs .. maximum xs]

--Recebe o board e o gaps
parseList :: [[Int]] -> [[Int]] -> Bool
parseList [] [] = True
parseList [] [[]] = True
parseList [[]] [] = True
parseList [[]] [[]] = True
parseList (a:b) (c:d) = if notBlack c then
                          if (noGaps a) then parseList b d
                          else False
                        else parseList b d

notBlack :: [Int] -> Bool 
notBlack (a:_) = if a == 1 then True else False

-- Generate a new version of board s with value x inserted at index p
tryWith :: Int -> [Int] -> [Int] -> Int -> [Int]
tryWith p s g x | (p == ((boardSize s) - 1)) && normalParse && transposedParse = take ((boardSize s) - 1) s ++ [x] ++ drop (boardSize s) s
                | (p /= ((boardSize s) - 1))                                   = take p s ++ [x] ++ drop (p + 1) s
                | otherwise                                   = []
  where normalParse =  ((parseList (splitManySizes (sizes (splitIn0and1 g)) s ) (splitIn0and1 g)) == True)
        transposedParse = ((parseList (splitManySizes (sizes (splitIn0and1Transposed g)) (joinTranspose (splitIn0and1Transposed s))) (splitIn0and1Transposed g)) == True)

--Junta o gapsBoard em uma lista unica, para poder ser usado no splitManySizes
joinTranspose :: [[a]] -> [a]
joinTranspose ([]:_) = []
joinTranspose [] = []
joinTranspose (a:b) = a ++ joinTranspose b

--Separa o gapsBoard em listas de 0 e 1
splitIn0and1 :: [Int] -> [[Int]]
splitIn0and1 a = separateZeros(chunksOf (lineWidth a) a)

--Separa a transposta do gapsBoard em listas de 0 e 1
splitIn0and1Transposed :: [Int] -> [[Int]]
splitIn0and1Transposed a = separateZeros(transpose (chunksOf (lineWidth a) a))

--Separa em listas de 0 e 1
separateZeros :: [[Int]] -> [[Int]]
separateZeros [] = []
separateZeros [[]] = [[]]
separateZeros (a:b) = split (dropBlanks . condense $  whenElt(<1)) a ++ separateZeros b

--Pega o tamanho de cada lista de 0 e 1 do gapsBoard, para usar depois para separar o board em tamanhos iguais.
sizes :: [[Int]] -> [Int]
sizes [[]] = []
sizes [] = []
sizes (a:b) = [length a] ++ sizes b

--recebe lista de tamanhos (sizes), recebe o tabuleiro, e retorna o tabuleiro splitado em tamanhos conforme a lista
splitManySizes :: [Int] -> [Int] -> [[Int]]
splitManySizes [] [] = [[]]
splitManySizes _ [] = [[]]
splitManySizes [] _ = [[]]
splitManySizes (a:b) d = [take a d] ++ splitManySizes b (drop a d)

-- Find the next blank value starting from index p on board s
nextBlank :: Int -> [Int] -> [Int] -> Int
nextBlank p s g | p == ((boardSize s)-1)           = ((boardSize s)-1)
                | (s !! (p + 1) == 0) && (g !! (p+1) == 1) = p + 1
                | otherwise         = nextBlank (p + 1) s g
              
-- Recursively try and brute-force solve the board given in s, starting at p,
-- with the set of possible solutions at that point.
solve :: Int -> [Int] -> [Int] -> [Int] -> [Int]
solve p s g h  | h == [] = []
               | (p == ((boardSize s) - 1)) && (tail h /= []) = []
               | (p == ((boardSize s) - 1)) && (tail h == []) = tryWith ((boardSize s)-1) s g (head h)
               | solvedNext == [] = solve p s g (tail h)
               | otherwise        = solvedNext                 
  where solveNext p s g = solve (nextBlank p s g) s g (solutionsAt (nextBlank p s g) s)
        solvedNext    = solveNext p (tryWith p s g (head h)) g

solveIt :: [Int] -> [Int] -> [[Int]]
solveIt s g = chunksOf (lineWidth s) (solve 0 s g (solutionsAt 0 s))

-- intersperse the element c through-out the string xs
joinWith :: a -> [a] -> [a]
joinWith _ (x:[])  = [x]
joinWith c (x:xs)  = x : c : joinWith c xs

print_ :: Show a => a -> IO ()
print_ x =  putStr $ (show x) ++ "\t" 

table :: (Foldable t, Show a) => [t a] -> IO ()
table xxs | length (nub [length xs | xs <- xxs])/=1 = error "not simetric"
          | otherwise = mapM_ printRow xxs 
            where printRow xs =  (mapM_ print_) xs >> putStrLn "" 