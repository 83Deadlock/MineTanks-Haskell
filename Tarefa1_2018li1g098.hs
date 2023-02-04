-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g098 where

import Data.List
import LI11819
import Tarefa0_2018li1g098
-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Move C, Move D, MudaTetromino, Move C, MudaTetromino, Move D],
            [Move C, Move D, MudaTetromino, Roda, MudaParede],
            [Move C, Desenha],
            [Move D, Desenha],
            [Move B,Desenha],
            [Move E, Desenha],
            [MudaTetromino, Roda, MudaParede, Move C, Desenha],
            [Move C,Move C, MudaTetromino, MudaParede, MudaParede,Roda,Desenha,Move D,Desenha],
            [Move C, MudaTetromino, Roda, MudaParede,Desenha],
            [Move D, MudaTetromino, Roda, MudaParede,Desenha],
            [Move B, MudaTetromino, Roda, MudaParede, Desenha],
            [Move E, MudaTetromino, Roda, MudaParede, Desenha]
           ]


-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.º

--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.  
--Testa o Move
instrucao (Move C) (Editor (x,y) dir tet par map) = Editor (x-1,y) dir tet par map
instrucao (Move D) (Editor (x,y) dir tet par map) = Editor (x,y+1) dir tet par map
instrucao (Move B) (Editor (x,y) dir tet par map) = Editor (x+1,y) dir tet par map
instrucao (Move E) (Editor (x,y) dir tet par map) = Editor (x,y-1) dir tet par map
--Testa o Roda 
instrucao (Roda) (Editor pos dir tet par map) = Editor pos (aux dir) tet par map
                                        where aux C = D
                                              aux D = B
                                              aux B = E
                                              aux E = C
--Testa o mudaTetronimo
instrucao (MudaTetromino) (Editor pos dir I par map) = Editor pos dir J par map 
instrucao (MudaTetromino) (Editor pos dir J par map) = Editor pos dir L par map
instrucao (MudaTetromino) (Editor pos dir L par map) = Editor pos dir O par map
instrucao (MudaTetromino) (Editor pos dir O par map) = Editor pos dir S par map
instrucao (MudaTetromino) (Editor pos dir S par map) = Editor pos dir T par map
instrucao (MudaTetromino) (Editor pos dir T par map) = Editor pos dir Z par map
instrucao (MudaTetromino) (Editor pos dir Z par map) = Editor pos dir I par map
--Testa o MudaParede
instrucao (MudaParede) (Editor pos dir tet Indestrutivel map) = Editor pos dir tet Destrutivel map
instrucao (MudaParede) (Editor pos dir tet Destrutivel map) = Editor pos dir tet Indestrutivel map
--Testa o Desenha
instrucao (Desenha) (Editor pos dir tet par map) = Editor pos dir tet par (aux1 pos dir tet par map (0,0))

-- Função auxiliar da Desenha, que em cada posição em que a Matriz do Tetromino é "True", altera o seu Bloco de Vazia para Parede Destrutivel ou Indestruttivel.
aux1 :: Posicao -> Direcao -> Tetromino -> Parede -> Mapa -> (Int,Int) -> Mapa
aux1 (x,y) dir tet par map (a,b) | (b == (length (tetDirToMap tet dir))) = map
                                 | (a < (length (tetDirToMap tet dir))) && ((encontraPosicaoMatriz (a,b) (tetDirToMap tet dir))) == False = aux1 (x + 1,y) dir tet par map (a + 1 ,b)
                                 | (a < (length (tetDirToMap tet dir))) && ((encontraPosicaoMatriz (a,b) (tetDirToMap tet dir))) == True = aux1 (x + 1,y) dir tet par (atualizaPosicaoMatriz (x,y) (Bloco par) map) (a + 1,b)
                                 | (a >= (length (tetDirToMap tet dir))) = aux1 (x-(length (tetDirToMap tet dir)),y+1) dir tet par map (0, b+1)

-- Função que roda o Tetromino para a direção pretendida
tetDirToMap :: Tetromino -> Direcao -> Matriz Bool
tetDirToMap tet dir | dir == C = tetrominoParaMatriz tet
                    | dir == D = rodaMatriz(tetrominoParaMatriz tet)
                    | dir == B = rodaMatriz (rodaMatriz (tetrominoParaMatriz tet))
                    | dir == E = rodaMatriz (rodaMatriz (rodaMatriz (tetrominoParaMatriz tet)))                     

-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes [] e = e            
instrucoes (h:t) (Editor pos dir tet par map) = instrucoes t(instrucao h (Editor pos dir tet par map))

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (x,0) = [[]]
mapaInicial (0,y) = [[]]
mapaInicial (1,y) = [replicate y (Bloco Indestrutivel)]
mapaInicial (x,1) = [Bloco Indestrutivel] : mapaInicial (x-1,1)
mapaInicial (x,y) = (replicate y (Bloco Indestrutivel)) : (aux2 (criaMatriz (x-2,y-2) Vazia) ++ [replicate y (Bloco Indestrutivel)])
                     where 
                        aux2 :: [[Peca]] -> [[Peca]]
                        aux2 (x:[]) = ([Bloco Indestrutivel] ++ x ++ [Bloco Indestrutivel]) : []
                        aux2 l = ([Bloco Indestrutivel] ++ head l ++ [Bloco Indestrutivel]) : aux2 (tail l)


-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial is = Editor (posicaoInicial is) C I Indestrutivel (mapaInicial (dimensaoInicial is))   
 
-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi is = mapaEditor (instrucoes is (editorInicial is))