module Str8ts where
import Data.List ( nub, transpose )
import Data.List.Split
    ( chunksOf, condense, dropBlanks, split, whenElt )

-- Declaração de tabuleiros de 5x5, 6x6, 7x7, 8x8 e 9x9. 
-- Os str8tsBoards representam o tabuleiro original, com a casa preta representada pelo -1.
-- Os gapsBoards são matrizes auxiliares, que representam se a casa é branca (1) ou preta (0).
str8tsBoard_5 :: [Int]
str8tsBoard_5 = [0, 1, -1, 0, 3,
                 1, 0, 3, 0, 0,
                 -1, -1, 4, 0, 5,
                 3, 0, 0, 2, 0,
                 4, 3, -1, 1, 2]

gapsBoard_5 :: [Int]
gapsBoard_5 = [1, 1, 0, 1, 1,
               1, 1, 1, 1, 1,
               0, 0, 1, 1, 0,
               1, 1, 1, 1, 1,
               1, 1, 0, 1, 1]

-- Exemplo retirado do site: https://www.janko.at/Raetsel/Straights/105.a.htm
str8tsBoard_6 :: [Int]
str8tsBoard_6 = [-1, 0, 0, 0, -1, 4,
                -1, 0, 0, 0, 0, 1,
                0, 0, 3, -1, 0, 0,
                5, 0, -1, -1, 0, 0,
                6, 0, 4, 0, 0, -1,
                -1, 1, 0, 0, 0, -1]

gapsBoard_6 :: [Int]
gapsBoard_6 = [0, 1, 1, 1, 0, 0,
              0, 1, 1, 1, 1, 1,
              1, 1, 0, 0, 1, 1,
              1, 1, 0, 0, 1, 1,
              1, 1, 1, 1, 1, 0,
              0, 0, 1, 1, 1, 0]

str8tsBoard_7 :: [Int]
str8tsBoard_7 = [1, 2, 0, 4, 7, 0, 6,
                 0, 0, 0, 0, -1, 6, 0,
                 -1, 0, 4, 7, 0, 1, -1,
                 5, 6, -1, -1, 3, 4, -1,
                 0, 0, 2, 0, 5, 0, 0,
                 6, 0, 0, 5, 0, 0, 3,
                 0, 5, -1, 2, 0, 7, -1]

gapsBoard_7 :: [Int]
gapsBoard_7 = [1, 1, 1, 1, 0, 1, 1,
               1, 1, 1, 1, 0, 1, 1,
               0, 1, 1, 0, 1, 1, 0,
               1, 1, 0, 0, 1, 1, 0,
               1, 1, 1, 1, 1, 1, 1,
               1, 1, 1, 1, 1, 1, 1,
               1, 1, 0, 0, 1, 1, 0]

str8tsBoard_8 :: [Int]
str8tsBoard_8 = [3, 4, 0, 2, -1, 8, 5, 0,
                 0, 1, 2, 0, 6, 0, 0, 0,
                 1, 0, 0, 4, 0, 6, 0, 0,
                 0, 3, -1, -1, 5, 0, 0, 7,
                 -1, 0, 0, 7, -1, 3, 2, 1,
                 5, 7, 4, 0, -1, -1, 0, 0,
                 0, 0, 7, -1, 0, 2, 1, 0,
                 8, 0, 0, -1, 2, 0, 4, 3]

gapsBoard_8 :: [Int]
gapsBoard_8 = [1, 1, 1, 1, 0, 0, 1, 1, 
               1, 1, 1, 1, 1, 1, 1, 1,
               1, 1, 1, 0, 1, 1, 1, 1,
               1, 1, 0, 0, 1, 1, 1, 1, 
               0, 1, 1, 1, 0, 1, 1, 1,
               1, 1, 1, 1, 0, 0, 1, 1,
               1, 1, 1, 0, 1, 1, 1, 1,
               0, 1, 1, 0, 1, 1, 1, 1]

str8tsBoard_9 :: [Int]
str8tsBoard_9 = [8, 9, -1, 6, 0, 5, 1, 2, -1,
                 9, 0, 7, 0, 8, 1, 2, 3, 0,
                 2, 5, 0, -1, -1, 0, 3, 1, 7,
                 -1, 0, 8, -1, 0, 3, -1, 4, 5,
                 5, 4, 9, 3, 1, 0, -1, 0, 6,
                 7, 8, -1, 0, 3, -1, 0, 5, -1,
                 4, -1, 2, 0, -1, -1, 7, 0, 3,
                 0, 1, 0, 2, 4, 7, 5, 0, 0,
                 -1, 0, 1, 7, 5, 0, 4, 9, 8]

gapsBoard_9 :: [Int]
gapsBoard_9 = [1, 1, 0, 1, 1, 0, 1, 1, 0,
               1, 1, 1, 1, 1, 1, 1, 1, 1,
               0, 1, 1, 0, 0, 1, 1, 0, 1,
               0, 1, 1, 0, 1, 1, 0, 1, 1,
               1, 1, 0, 1, 1, 1, 0, 1, 1,
               1, 1, 0, 1, 1, 0, 1, 1, 0,
               1, 0, 1, 1, 0, 0, 1, 1, 0,
               1, 1, 1, 1, 1, 1, 1, 1, 1,
               0, 1, 1, 0, 1, 1, 0, 1, 1]

-- Função que retorna o tamanho do tabuleiro de entrada.
boardSize :: [Int] -> Int
boardSize [] = 0
boardSize s = length(s)

-- Realiza raiz quadrada no tamanho do tabuleiro, para descobrir largura da linha.
lineWidth :: [Int] -> Int
lineWidth [] = 0
lineWidth s = intSquareRoot (boardSize s)

-- Retorna valor inteiro de uma raiz quadrada.
intSquareRoot :: Int -> Int
intSquareRoot n = try n where
  try i   | i*i > n   = try (i - 1) 
          | i*i <= n  = i

-- Converte índice i, com largura de linha l, de uma célula em coordenada x y
indexToCoord :: Int -> Int -> (Int, Int)
indexToCoord i l = (calcX i l, calcY i l)
  where calcX i l  = i - l * (i `div` l)
        calcY i l  = i `div` l

-- Converte as coordenadas x e y em índice do tabuleiro com largura de linha l
coordToIndex :: (Int, Int) -> Int -> Int
coordToIndex (x, y) l = x + y * l

-- Retorna a coluna de valores do tabuleiro s no índice p.
columnAt :: Int -> [Int] -> [Int]
columnAt p s = helperColumnAt (indexToCoord p (lineWidth s)) s
  where helperColumnAt (x, _) s = map (\y -> s !! coordToIndex (x, y) (lineWidth s)) [0..((lineWidth s)-1)] 

-- Retorna a linha de valores do tabuleiro s no índice p.
rowAt :: Int -> [Int] -> [Int]
rowAt p s = helperRowAt (indexToCoord p (lineWidth s)) s
  where helperRowAt (_, y) s = map (\x -> s !! coordToIndex (x, y) (lineWidth s)) [0..((lineWidth s)-1)]

-- Remove os elementos da segunda lista, a partir da primeira lista
remove :: [Int] -> [Int] -> [Int]
remove [] _       = []
remove xs []      = xs
remove xs (y:ys)  = remove (removeAll y xs) ys

-- Remove todas as ocorrências de um valor y na lista de entrada
removeAll :: Int -> [Int] -> [Int]
removeAll _ []     = []
removeAll y (x:xs) | x == y    = removeAll y xs
                   | otherwise = x : removeAll y xs

-- Retorna a lista de soluções no índice p do tabuleiro s
solutionsAt :: Int -> [Int] -> [Int]
solutionsAt p s | p > length s  = []
                | ((s !! p) == 0) = [1..(lineWidth s)] `remove` (columnAt p s ++ rowAt p s)
                | otherwise     = [s !! p]

-- Verifica se a os valores da lista representam uma sequência
noGaps :: (Enum a, Ord a) => [a] -> Bool
noGaps [] = True
noGaps xs = all (`elem` xs) [minimum xs .. maximum xs]

-- Recebe o tabuleiro resolvido até o momento e o gapsBoard (matriz de 0 e 1 original).
-- Com eles, Verifica se os valores analisados pertencem a células brancas e, se sim, 
-- analisa se faz parte de uma sequência.
-- Caso sejam células pretas, não realiza o teste, pois células pretas não fazem parte da sequência. 
parseList :: [[Int]] -> [[Int]] -> Bool
parseList [] [] = True
parseList [] [[]] = True
parseList [[]] [] = True
parseList [[]] [[]] = True
parseList (a:b) (c:d) = if notBlack c then
                          if (noGaps a) then parseList b d
                          else False
                        else parseList b d

-- Retorna se a lista recebida possui células brancas.
notBlack :: [Int] -> Bool 
notBlack (a:_) = if a == 1 then True else False

-- Gera uma nova versão do tabuleiro s com o valor x inserido no índice p com o tabuleiro auxiliar gapsBoard g. 
-- Faz a verificação de sequência das linhas e das colunas
tryWith :: Int -> [Int] -> [Int] -> Int -> [Int]
tryWith p s g x | (p == ((boardSize s) - 1)) && normalParse && transposedParse = take ((boardSize s) - 1) s ++ [x] ++ drop (boardSize s) s
                | (p /= ((boardSize s) - 1))                                   = take p s ++ [x] ++ drop (p + 1) s
                | otherwise                                   = []
  where normalParse =  ((parseList (splitManySizes (sizes (splitIn0and1 g)) s ) (splitIn0and1 g)) == True)
        transposedParse = ((parseList (splitManySizes (sizes (splitIn0and1Transposed g)) (joinTranspose (splitIn0and1Transposed s))) (splitIn0and1Transposed g)) == True)

--Separa o gapsBoard em listas de tamanho da largura da linha, e depois separa em listas contendo apenas 0 e listas com apenas 1.
splitIn0and1 :: [Int] -> [[Int]]
splitIn0and1 a = separateZeros(chunksOf (lineWidth a) a)

--Separa a transposta do gapsBoard em listas de tamanho da largura da linha, e depois separa em listas contendo apenas 0 e listas com apenas 1.
splitIn0and1Transposed :: [Int] -> [[Int]]
splitIn0and1Transposed a = separateZeros(transpose (chunksOf (lineWidth a) a))

--Separa uma lista de listas com 0 e 1 misturado em uma lista contendo listas com apenas 0 e listas com apenas 1.
separateZeros :: [[Int]] -> [[Int]]
separateZeros [] = []
separateZeros [[]] = [[]]
separateZeros (a:b) = split (dropBlanks . condense $  whenElt(<1)) a ++ separateZeros b

--Realiza a contagem de elementos de cada lista dentro de uma lista, e retorna uma lista com o valor dos elementos.
sizes :: [[Int]] -> [Int]
sizes [[]] = []
sizes [] = []
sizes (a:b) = [length a] ++ sizes b

--Recebe lista de tamanhos (sizes), recebe o tabuleiro, e retorna o tabuleiro dividido em tamanhos conforme a lista recebida.
splitManySizes :: [Int] -> [Int] -> [[Int]]
splitManySizes [] [] = [[]]
splitManySizes _ [] = [[]]
splitManySizes [] _ = [[]]
splitManySizes (a:b) d = [take a d] ++ splitManySizes b (drop a d)

--Junta o gapsBoard em uma lista unica, ao invés de uma lista de listas, para poder ser utilizado no splitManySizes
joinTranspose :: [[a]] -> [a]
joinTranspose ([]:_) = []
joinTranspose [] = []
joinTranspose (a:b) = a ++ joinTranspose b

-- Encontra próximo valor nulo começando no índice p do tabuleiro s
nextBlank :: Int -> [Int] -> [Int] -> Int
nextBlank p s g | p == ((boardSize s)-1)           = ((boardSize s)-1)
                | (s !! (p + 1) == 0) && (g !! (p+1) == 1) = p + 1
                | otherwise         = nextBlank (p + 1) s g

-- Tenta recursivamente resolver o tabuleiro s, começando em p, o gapsBoard representado pelo g
-- com todas as possibilidades naquele ponto              
solve :: Int -> [Int] -> [Int] -> [Int] -> [Int]
solve p s g h  | h == [] = []
               | (p == ((boardSize s) - 1)) && (tail h /= []) = []
               | (p == ((boardSize s) - 1)) && (tail h == []) = tryWith ((boardSize s)-1) s g (head h)
               | solvedNext == [] = solve p s g (tail h)
               | otherwise        = solvedNext                 
  where solveNext p s g = solve (nextBlank p s g) s g (solutionsAt (nextBlank p s g) s)
        solvedNext    = solveNext p (tryWith p s g (head h)) g

-- Função de chamada inicial da resolução do código.
-- Transforma a lista de entrada em uma matriz, e chama o solve para resolver a solução recursivamente em backtracking.
solveIt :: [Int] -> [Int] -> [[Int]]
solveIt s g = chunksOf (lineWidth s) (solve 0 s g (solutionsAt 0 s))

-- Realiza a impressão do tabuleiro resolvido na tela.
print_ :: Show a => a -> IO ()
print_ x =  putStr $ (show x) ++ "\t" 

-- Formata a solução encontrada para ser exibida na tela.
table :: (Foldable t, Show a) => [t a] -> IO ()
table xxs | length (nub [length xs | xs <- xxs])/=1 = error "not simetric"
          | otherwise = mapM_ printRow xxs 
            where printRow xs =  (mapM_ print_) xs >> putStrLn "" 