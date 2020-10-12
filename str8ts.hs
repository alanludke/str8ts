-- Wynand van Dyk - September 2012
-- Recursive backtracking straights solver
-- 
-- Load this file up in ghci with the following command:
-- :l straights.hs
--
-- Run the solver on the puzzle and print out the solution:
-- putStrLn $ pPrint $ solveIt straights

module Straights where
import Data.List
import Data.List.Split

--The puzzle we are trying to solve
straights2 :: [Int]
straights2 = [4, 9, 0, 0, 1, 9,
             0, 0, 0, 0, 0, 9,
             0, 0, 9, 0, 0, 5,
             2, 0, 0, 9, 0, 0,
             9, 3, 0, 0, 0, 6,
             9, 0, 0, 0, 9, 1]

answer :: [Int]
answer = [4, 9, 2, 3, 1, 9,
          3, 5, 1, 2, 4, 9,
          1, 2, 9, 4, 3, 5,
          2, 1, 3, 9, 5, 4,
          9, 3, 4, 5, 2, 6,
          9, 4, 5, 6, 9, 1]

gapsBoard2 :: [Int]
gapsBoard2 = [0, 0, 1, 1, 1, 0,
             1, 1, 1, 1, 1, 0,
             1, 1, 0, 1, 1, 1,
             1, 1, 1, 0, 1, 1,
             0, 1, 1, 1, 1, 1,
             0, 1, 1, 1, 0, 0]


gapsBoard3 :: [Int]
gapsBoard3 = [0, 1, 1, 1, 0, 0,
              0, 1, 1, 1, 1, 1,
              1, 1, 0, 0, 1, 1,
              1, 1, 0, 0, 1, 1,
              1, 1, 1, 1, 1, 0,
              0, 0, 1, 1, 1, 0]

straights :: [Int]
straights = [9, 0, 0, 1, 9, 9,
            9, 0, 0, 0, 5, 0,
            9, 0, 1, 0, 0, 0,
            4, 0, 0, 0, 0, 9,
            0, 6, 5, 0, 0, 9,
            9, 9, 9, 0, 1, 4]

gapsBoard :: [Int]
gapsBoard = [0, 1, 1, 0, 0, 0,
             0, 1, 1, 1, 1, 1,
             0, 1, 1, 1, 1, 1,
             1, 1, 1, 1, 1, 0,
             1, 1, 1, 1, 1, 0,
             0, 0, 0, 1, 1, 0]

-- converts an index i into an x and y co-ordinate
itop :: Int -> (Int, Int)
itop i = (calcX i, calcY i)
  where calcX i   = i - 6 * (i `div` 6)
        calcY i   = i `div` 6

-- Takes an x and y co-ordinate and converts it into an index
ptoi :: (Int, Int) -> Int
ptoi (x, y) = x + y * 6

-- Retrieves the vertical column of values from the board (s) at the index (p)
columnAt :: Int -> [Int] -> [Int]
columnAt p s = helperColumnAt (itop p) s
  where helperColumnAt (x, _) s = map (\y -> s !! ptoi (x, y)) [0..5]

-- Retrieves the horizontal row of values from the board (s) at the index (p)
rowAt :: Int -> [Int] -> [Int]
rowAt p s = helperRowAt (itop p) s
  where helperRowAt (_, y) s = map (\x -> s !! ptoi (x, y)) [0..5]


-- Removes the elements in the second list from the first list
-- first list - second list
-- remove [9,0,0,0,9,1] [9,9] = [0,0,0,1]
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
                | ((s !! p) == 0) = [1..6] `remove` (columnAt p s ++ rowAt p s)
                | otherwise     = [s !! p]

--[n | n <- [1..6], not $ any (n==) (getRow index board), not $ any (n==) (getColumn index board)]
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
notBlack (a:b) = if a == 1 then True else False

-- Generate a new version of board s with value x inserted at index p
tryWith :: Int -> [Int] -> Int -> [Int]
tryWith 35 s x | ((parseList (splitManySizes (sizes (splitIn0and1 gapsBoard)) s ) (splitIn0and1 gapsBoard)) == True) && 
                 ((parseList (splitManySizes (sizes (splitIn0and1Transposed gapsBoard)) (joinTranspose (splitIn0and1Transposed s)) ) (splitIn0and1Transposed gapsBoard)) == True)  = take 35 s ++ [x] ++ drop (35 + 1) s
               | otherwise        = []
tryWith p s x = take p s ++ [x] ++ drop (p + 1) s

--Junta o gapsBoard em uma lista unica, para poder ser usado no splitManySizes
joinTranspose :: [[a]] -> [a]
joinTranspose ([]:_) = []
joinTranspose [] = []
joinTranspose (a:b) = a ++ joinTranspose b

--Separa o gapsBoard em listas de 0 e 1
splitIn0and1 :: [Int] -> [[Int]]
splitIn0and1 a = separateZeros(chunksOf 6(a))

--Separa a transposta do gapsBoard em listas de 0 e 1
splitIn0and1Transposed :: [Int] -> [[Int]]
splitIn0and1Transposed a = separateZeros(transpose (chunksOf 6(a)))

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
-- 35 is the index of the last element in s
nextBlank :: Int -> [Int] -> Int
nextBlank p s | p == 35           = 35
              | (s !! (p + 1) == 0) && (gapsBoard !! (p+1) == 1) = p + 1
              | otherwise         = nextBlank (p + 1) s
              
-- Recursively try and brute-force solve the board given in s, starting at p,
-- with the set of possible solutions at that point.
-- 35 is the index of the last element in s
solve :: Int -> [Int] -> [Int] -> [Int]
solve 35 s []     = []
solve 35 s (x:[]) = tryWith 35 s x
solve 35 s (x:_)  = []
solve _  s []     = []
solve p s (x:xs)  | solvedNext == [] = solve p s xs
                  | otherwise        = solvedNext                 
  where solveNext p s = solve (nextBlank p s) s (solutionsAt (nextBlank p s) s)
        solvedNext    = solveNext p (tryWith p s x)

solveIt s = solve 0 s (solutionsAt 0 s)

-- intersperse the element c through-out the string xs
joinWith :: a -> [a] -> [a]
joinWith _ (x:[])  = [x]
joinWith c (x:xs)  = x : c : joinWith c xs

-- Pretty-print the board as a spaced out 6 x 6 square
pPrint [] = []
pPrint s  = spaceOut s ++ pPrint (drop 6 s)
  where showS s    = concatMap show s
        space      = ' '
        newline    = "\n"
        spaceOut s = joinWith space (take 6 (showS s) ++ newline)
   
   