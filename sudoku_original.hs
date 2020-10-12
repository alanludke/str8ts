-- Wynand van Dyk - September 2012
-- Recursive backtracking sudoku solver
-- 
--
-- Load this file up in ghci with the following command:
-- :l sudoku.hs
--
-- Run the solver on the puzzle and print out the solution:
-- putStrLn $ pPrint $ solveIt sudoku
--
-- The result should look like this (for the default puzzle)
--
-- 8 6 1 3 4 7 2 9 5 
-- 4 3 2 8 9 5 1 6 7 
-- 9 5 7 1 6 2 4 8 3 
-- 2 7 8 4 5 1 6 3 9 
-- 5 4 9 6 8 3 7 2 1 
-- 6 1 3 2 7 9 8 5 4 
-- 3 2 4 9 1 8 5 7 6 
-- 1 8 5 7 3 6 9 4 2 
-- 7 9 6 5 2 4 3 1 8 

module Sudoku where

-- The puzzle we are trying to solve
sudoku :: [Int]
sudoku = [8, 0, 1, 3, 4, 0, 0, 0, 0,
          4, 3, 0, 8, 0, 0, 1, 0, 7,
          0, 0, 0, 0, 6, 0, 0, 0, 3,
          2, 0, 8, 0, 5, 0, 0, 0, 9,
          0, 0, 9, 0, 0, 0, 7, 0, 0,
          6, 0, 0, 0, 7, 0, 8, 0, 4,
          3, 0, 0, 0, 1, 0, 0, 0, 0,
          1, 0, 5, 0, 0, 6, 0, 4, 2,
          0, 0, 0, 0, 2, 4, 3, 0, 8]

-- converts an index i into an x and y co-ordinate
itop :: Int -> (Int, Int)
itop i = (calcX i, calcY i)
  where calcX i   = i - 9 * (i `div` 9)
        calcY i   = i `div` 9

-- Takes an x and y co-ordinate and converts it into an index
ptoi :: (Int, Int) -> Int
ptoi (x, y) = x + y * 9

-- Retrieves the vertical column of values from the board (s) at the index (p)
columnAt :: Int -> [Int] -> [Int]
columnAt p s = helperColumnAt (itop p) s
  where helperColumnAt (x, _) s = map (\y -> s !! ptoi (x, y)) [0..8]

-- Retrieves the horizontal row of values from the board (s) at the index (p)
rowAt :: Int -> [Int] -> [Int]
rowAt p s = helperRowAt (itop p) s
  where helperRowAt (_, y) s = map (\x -> s !! ptoi (x, y)) [0..8]

-- Retrieves the 3 x 3 square of values from the board (s) at the index (p)
squareAt :: Int -> [Int] -> [Int]
squareAt p s = helperSquareAt (itop p) s
  where helperSquareAt (x, y) s = [ s !! ptoi (xx + sqOffsetX x, yy + sqOffsetY y) | xx <- [0..2], yy <- [0..2] ] 
        sqOffsetX x' = 3 * (x' `div` 3)
        sqOffsetY y' = 3 * (y' `div` 3)

-- Removes the elements in the second list from the first list
remove' :: [Int] -> [Int] -> [Int]
remove' [] _       = []
remove' xs []      = xs
remove' xs (y:ys)  = remove' (removeAll y xs) ys

-- Remove all occurences of a value in a list
removeAll :: Int -> [Int] -> [Int]
removeAll _ []     = []
removeAll y (x:xs) | x == y    = removeAll y xs
                   | otherwise = x : removeAll y xs

-- The list of solutions at the index p of board s
solutionsAt :: Int -> [Int] -> [Int]
solutionsAt p s | p > length s  = []
                | (s !! p) == 0 = [1..9] `remove'` (columnAt p s ++ rowAt p s ++ squareAt p s)
                | otherwise     = [s !! p]

-- Generate a new version of board s with value x inserted at index p
tryWith :: Int -> [Int] -> Int -> [Int]
tryWith p s x = take p s ++ [x] ++ drop (p + 1) s

-- Find the next blank value starting from index p on board s
-- 80 is the index of the last element in s
nextBlank :: Int -> [Int] -> Int
nextBlank p s | p == 80           = 80
              | s !! (p + 1) == 0 = p + 1
              | otherwise         = nextBlank (p + 1) s

-- Recursively try and brute-force solve the board given in s, starting at p,
-- with the set of possible solutions at that point.
-- 80 is the index of the last element in s
solve :: Int -> [Int] -> [Int] -> [Int]
solve 80 s []     = []
solve 80 s (x:[]) = tryWith 80 s x
solve 80 s (x:_)  = []
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

-- Pretty-print the board as a spaced out 9 x 9 square
pPrint [] = []
pPrint s  = spaceOut s ++ pPrint (drop 9 s)
  where showS s    = concatMap show s
        space      = ' '
        newline    = "\n"
        spaceOut s = joinWith space (take 9 (showS s) ++ newline) 