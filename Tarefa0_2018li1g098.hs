-- | Description : Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.

module Tarefa0_2018li1g098 where 
import LI11819
import Data.List

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre Vetores

-- *** Funções gerais sobre 'Vetor'es

-- | Soma dois 'Vetor'es
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (u1,u2)(v1,v2) = ((u1+v1),(u2+v2))

-- | Subtrai dois 'Vetor'es
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (u1,u2)(v1,v2) = ((u1-v1),(u2-v2))

-- | Multiplica um escalar por um 'Vetor'
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor k (x,y) = (k*x,k*y)

-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distância à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (x,y) = (y,-x)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH
inverteVetorH :: Vetor -> Vetor
inverteVetorH (x,y) = (x,-y)

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http.oi68.tinypic.com/2n7fqxy.jpg inverteVetorV
inverteVetorV :: Vetor -> Vetor
inverteVetorV (x,y) = (-x,y)

-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor dir | dir == C = (-1,0)
                     | dir == B = (1,0)
                     | dir == D = (0,1)
                     | dir == E = (0,-1)

-- ** Funções sobre listas.

-- *** Funções gerais sobre listas
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade

-- | Verifica se o indice pertence à lista.

eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido _ [] = False
eIndiceListaValido i l = i >= 0 && i < length l 

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- >>https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
--__NB:__Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz a | length a == 0 || length (head a) == 0 = (0,0)
                 | otherwise = (length a, length (head a))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida (p1,p2) a = (p1 >= 0) && (p1 <= length a) && (p2 >= 0) && (p2 < length (head a))

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz (x,y) a | x == 0 = True
                     | y == 0 = True
                     | x == length a - 1 = True
                     | y == length (head a) - 1 = True
                     |otherwise = False

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz I = replicate 4 [False,True,False,False]
tetrominoParaMatriz J = [
                         [False,True,False],
                         [False,True,False],
                         [True,True,False]
                        ]
tetrominoParaMatriz L = [
                         [False,True,False],
                         [False,True,False],
                         [False,True,True]
                        ]
tetrominoParaMatriz O = replicate 2 [True,True]
tetrominoParaMatriz S = [
                         [False,True,True],
                         [True,True,False],
                         [False,False,False]
                        ]
tetrominoParaMatriz T = [
                         [False,False,False],
                         [True,True,True],
                         [False,True,False]
                        ]
tetrominoParaMatriz Z = [
                         [True,True,False],
                         [False,True,True],
                         [False,False,False]
                        ]
-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 l = head l
encontraIndiceLista i (h:t) = encontraIndiceLista (i-1) t 

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista i e l | eIndiceListaValido i l = aux1 i e l
                          | otherwise = l
                          where aux1 _ _ [] = []
                                aux1 i e (h:t) | i == 0 = (e:t)
                                               |otherwise = h:(aux1 (i-1) e t)

-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a -> Matriz a
rodaMatriz m = transpose (reverse m)  

-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH [] = []
inverteMatrizH (h:t) = (reverse h):(inverteMatrizH t)

-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV [] = []
inverteMatrizV m = reverse m  
    
-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (0,b) _ = []
criaMatriz (a,b) x = (aux b x):(criaMatriz (a-1,b) x)
           where aux 0 _ = []
                 aux b x = x:(aux (b-1) x)

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (x,y) m = encontraIndiceLista y (encontraIndiceLista x m)

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = [] 
atualizaPosicaoMatriz (x,y) a m | ePosicaoMatrizValida (x,y) m = aux2 (x,y) a m
                                | otherwise = m
                        where aux2 (0,y) a (h:t) = (atualizaIndiceLista y a h):t
                              aux2 _ _ [] = []
                              aux2 (x,y) m (h:t)= h:(atualizaPosicaoMatriz (x-1,y) a t)