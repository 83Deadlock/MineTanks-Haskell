-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g098 where

import Data.List
import LI11819
import Tarefa0_2018li1g098
import Tarefa1_2018li1g098

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0 , (Movimenta D) , (Estado (mapaInicial (12,12)) [(Jogador (2,2) D 3 3 2),(Jogador (2,9) B 3 3 2),(Jogador (4,2) C 3 3 2),(Jogador (6,6) E 3 3 2)] [])),
            (0 , (Dispara Laser) , (Estado (mapaInicial (12,12)) [(Jogador (2,3) D 3 3 2),(Jogador (2,9) B 3 3 2),(Jogador (4,2) C 3 3 2),(Jogador (6,6) E 3 3 2)] [])),
            (1 , (Movimenta E) , (Estado (mapaInicial (12,12)) [(Jogador (2,3) D 3 2 2),(Jogador (2,9) E 2 3 2),(Jogador (4,2) C 3 3 2),(Jogador (6,6) E 3 3 2)] [(DisparoLaser 0 (posBala (2,3) D) D)])),
            (2 , (Dispara Choque) , (Estado (mapaInicial (12,12)) [(Jogador (2,3) D 3 2 2),(Jogador (2,8) E 2 3 2),(Jogador (4,2) C 3 3 2),(Jogador (6,6) E 3 3 2)] [(DisparoLaser 0 (posBala (2,3) D) D)])),
            (3 , (Movimenta E) , (Estado (mapaInicial (12,12)) [(Jogador (2,3) D 3 2 2),(Jogador (2,8) E 2 3 2),(Jogador (4,2) C 3 3 1),(Jogador (6,6) E 3 3 2)] [(DisparoChoque 2 5),(DisparoLaser 0 (posBala (2,3) D) D)])),
            (0 , (Movimenta D) , (Estado (mapaInicial (12,12)) [(Jogador (2,3) D 3 2 2),(Jogador (2,8) E 2 3 2),(Jogador (4,2) C 3 3 1),(Jogador (6,5) E 3 3 2)] [(DisparoChoque 2 5),(DisparoLaser 0 (posBala (2,3) D) D)])),
            (3 , (Movimenta C) , (Estado (mapaInicial (12,12)) [(Jogador (2,4) D 3 2 2),(Jogador (2,8) E 2 3 2),(Jogador (4,2) C 3 3 1),(Jogador (6,5) E 3 3 2)] [(DisparoChoque 2 5),(DisparoLaser 0 (posBala (2,3) D) D)])),
            (3 , (Movimenta C) , (Estado (mapaInicial (12,12)) [(Jogador (2,4) D 3 2 2),(Jogador (2,8) E 2 3 2),(Jogador (4,2) C 3 3 1),(Jogador (6,5) C 3 3 2)] [(DisparoChoque 2 5),(DisparoLaser 0 (posBala (2,3) D) D)])),
            (3 , (Movimenta D) , (Estado (mapaInicial (12,12)) [(Jogador (2,4) D 3 2 2),(Jogador (2,8) E 2 3 2),(Jogador (4,2) C 3 3 1),(Jogador (5,5) C 3 3 2)] [(DisparoChoque 2 5),(DisparoLaser 0 (posBala (2,3) D) D)])),
            (3 , (Movimenta D) , (Estado (mapaInicial (12,12)) [(Jogador (2,4) D 3 2 2),(Jogador (2,8) E 2 3 2),(Jogador (4,2) C 3 3 1),(Jogador (5,5) D 3 3 2)] [(DisparoChoque 2 5),(DisparoLaser 0 (posBala (2,3) D) D)])),
            (3 , (Movimenta B) , (Estado (mapaInicial (12,12)) [(Jogador (2,4) D 3 2 2),(Jogador (2,8) E 2 3 2),(Jogador (4,2) C 3 3 1),(Jogador (5,6) D 3 3 2)] [(DisparoChoque 2 5),(DisparoLaser 0 (posBala (2,3) D) D)])),
            (3 , (Movimenta B) , (Estado (mapaInicial (12,12)) [(Jogador (2,4) D 3 2 2),(Jogador (2,8) E 2 3 2),(Jogador (4,2) C 3 3 1),(Jogador (5,5) B 3 3 2)] [(DisparoChoque 2 5),(DisparoLaser 0 (posBala (2,3) D) D)])),
            (3 , (Movimenta E) , (Estado (mapaInicial (12,12)) [(Jogador (2,4) D 3 2 2),(Jogador (2,8) E 2 3 2),(Jogador (4,2) C 3 3 1),(Jogador (6,5) B 3 3 2)] [(DisparoChoque 2 5),(DisparoLaser 0 (posBala (2,3) D) D)]))]

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.

-- | Efetua a jogada 'Movimenta' numa certa 'Direção' se o 'Jogador' tiver vidas, caminho livre e se estiver virado para a 'Direcao' indicada. Se não estiver, apenas roda o 'Jogador' nessa mesma 'Direção'.
jogada j (Movimenta dir) (Estado map js ds) | temVidas (selecionaJogador j js) == True && direcaoCerta (selecionaJogador j js) dir == True && ((obstaculo (selecionaJogador j js) (Estado map js ds)) == True) && ((temChoque j (selecionaJogador j js) (Estado map js ds)) == False) = Estado map (atualizaIndiceLista (j) (alteraPos (selecionaJogador j js) dir) js) ds
                                            | ((temVidas (selecionaJogador j js)) == True) && ((direcaoCerta (selecionaJogador j js) dir) == False) = Estado map (atualizaIndiceLista (j) (alteraDirJ (selecionaJogador j js) dir) js) ds
                                            | otherwise = Estado map js ds -- ^ Se o 'Jogador' não tiver vidas, tiver obstáculos no seu caminho ou adversários que o impeçam de mover, a 'Jogada' não é efetuada.

-- | Efetua a jogada 'Dispara' com uma certa 'Arma' se o 'Jogador' vidas e munições.
jogada j (Dispara arma) (Estado map js ds) | (temVidas (selecionaJogador j js) == True) && (verificaBalas (selecionaJogador j js) arma == True) = Estado map (atualizaIndiceLista (j) (alteraBalas (selecionaJogador j js) arma) js) ([(estadoDisparo j (selecionaJogador j js) arma)] ++ ds) -- ^ Ao efetuar um 'Disparo', o 'Jogador' perde uma munição do tipo de 'Arma' que disparou e adiciona-se o 'Disparo' ao 'Estado de Disparos' no 'Estado' do jogo. 
                                           | otherwise = Estado map js ds -- ^ Se o jogador não tiver vidas ou munições, não efetuará a 'Jogada', logo o 'Estado' do jogo mantém-se.

-- | Função que determina o 'Estado' do 'Disparo' ('jogadorDisparo', 'posicaoDisparo' e 'direcaoDisparo').
estadoDisparo :: Int -> Jogador -> Arma -> Disparo
estadoDisparo j (Jogador (x,y) dirJ v l c) arma | arma == Canhao = DisparoCanhao j (posBala (x,y) dirJ) dirJ
                                                | arma == Laser = DisparoLaser j (posBala (x,y) dirJ) dirJ
                                                | otherwise = DisparoChoque j 5

-- | Funcao para determinar a 'Posicao' da bala após o 'Disparo'.
posBala :: PosicaoGrelha -> Direcao -> PosicaoGrelha
posBala (x,y) dirJ | dirJ == C = (x - 1 , y)
                   | dirJ == D = (x , y + 1)
                   | dirJ == B = (x + 1 , y)
                   | dirJ == E = (x , (y - 1))

-- | Função usada para retirar uma bala da 'Arma' usada no disparo ao 'Jogador' correspondente.
alteraBalas :: Jogador -> Arma -> Jogador
alteraBalas (Jogador pos dirJ vidas lasers choques) arma | arma == Choque = Jogador pos dirJ vidas lasers (choques-1)
                                                         | arma == Laser = Jogador pos dirJ vidas (lasers-1) choques
                                                         | otherwise = Jogador pos dirJ vidas lasers choques

-- | Verifica se o 'Jogador' tem balas na 'Arma' que pretende disparar.
verificaBalas :: Jogador -> Arma -> Bool
verificaBalas (Jogador pos dirJ vidas lasers choques) Canhao = True
verificaBalas (Jogador pos dirJ vidas lasers choques) Laser = lasers > 0
verificaBalas (Jogador pos dirJ vidas lasers choques) Choque = choques > 0 

-- | Função que vai à lista de 'Estado's dos 'Jogador'es e retira o 'Estado' do 'Jogador' selecionado.
selecionaJogador :: Int -> [Jogador] -> Jogador 
selecionaJogador j js = (!!) js j

-- | Altera a 'Posicao' de um 'Jogador' na 'Jogada Movimenta'
alteraPos :: Jogador -> Direcao -> Jogador
alteraPos (Jogador (x,y) dirJ vidas lasers choques) dir | dir == C = Jogador (x-1,y) dirJ vidas lasers choques
                                                        | dir == D = Jogador (x,y+1) dirJ vidas lasers choques
                                                        | dir == B = Jogador (x+1,y) dirJ vidas lasers choques
                                                        | dir == E = Jogador (x,y-1) dirJ vidas lasers choques

-- | Altera a 'Direcao' de um 'Jogador' caso esta não seja a do movimento pretendido.
alteraDirJ :: Jogador -> Direcao -> Jogador
alteraDirJ (Jogador pos dirJ vidas lasers choques) dir = Jogador pos dir vidas lasers choques

-- | Indica se o número de vidas de um certo jogador igual ou inferior a zero.
temVidas :: Jogador -> Bool
temVidas (Jogador posJ dirJ vidas lasers choques) | vidas > 0 = True
                                                  | vidas == 0 = False
                                                  | otherwise = error "Invalid Input"

-- | Verifica se o jogador está virado para onde quer andar.
direcaoCerta :: Jogador -> Direcao -> Bool
direcaoCerta (Jogador posJ dirJ vidas lasers choques) dir = dirJ == dir

-- | Verifica se o jogador tem um adversário ou um bloco à sua frente.
obstaculo :: Jogador -> Estado -> Bool
obstaculo (Jogador (xj,yj) dirJ vidas lasers choques) (Estado m js ds) | dirJ == C = ((encontraPosicaoMatriz (xj-1,yj) m) /= Vazia) || (temAdversarioPerto (Jogador (xj,yj) dirJ vidas lasers choques) (Estado m js ds))
                                                                       | dirJ == D = ((encontraPosicaoMatriz (xj,yj+1) m) /= Vazia) || (temAdversarioPerto (Jogador (xj,yj) dirJ vidas lasers choques) (Estado m js ds))
                                                                       | dirJ == B = ((encontraPosicaoMatriz (xj+1,yj) m) /= Vazia) || (temAdversarioPerto (Jogador (xj,yj) dirJ vidas lasers choques) (Estado m js ds))
                                                                       | otherwise = ((encontraPosicaoMatriz (xj-1,yj) m) /= Vazia) || (temAdversarioPerto (Jogador (xj,yj) dirJ vidas lasers choques) (Estado m js ds))

-- | Verifica se o jogador selecionado tem algum adversário perto que o impeça de se mover.                            
temAdversarioPerto :: Jogador -> Estado -> Bool
temAdversarioPerto (Jogador (x,y) dirJ vidas lasers choques) (Estado m js ds) | dirJ == C = listaPosicoesC `intersect` listaPosicoesJ == []
                                                                              | dirJ == D = listaPosicoesD `intersect` listaPosicoesJ == []
                                                                              | dirJ == B = listaPosicoesB `intersect` listaPosicoesJ == []
                                                                              | dirJ == E = listaPosicoesE `intersect` listaPosicoesJ == [] 
                                                                                  where listaPosicoesC = [(x-2,y-1),(x-2,y),(x-2,y+1)]
                                                                                        listaPosicoesD = [(x-1,y+2),(x,y+2),(x+1,y+2)]
                                                                                        listaPosicoesB = [(x+2,y-1),(x+2,y),(x+2,y+1)]
                                                                                        listaPosicoesE = [(x-1,y-2),(x,y-2),(x+1,y-2)] 
                                                                                        listaPosicoesJ = (delete (x,y) (posicoesJogadores js))


-- |Cria uma 'Lista' com as 'Posicoes' dos 'Jogador'es
posicoesJogadores :: [Jogador] -> [PosicaoGrelha]
posicoesJogadores [] = []
posicoesJogadores ((Jogador (x,y) v d l c):t) = (x,y):(posicoesJogadores t)

-- | Verifica se o 'Jogador' está numa área afetada por 'Choque'.
temChoque :: Int -> Jogador -> Estado -> Bool
temChoque j (Jogador (x,y) d v l c) (Estado m js ds) = (houveChoques ds) && (elem (x,y) (areaChoque (posicoesJogadores (jogadorChoque js ds))))

-- |Verifica se já houve algum 'Choque' a ser jogado.
houveChoques :: [Disparo] -> Bool
houveChoques [] = False
houveChoques ((DisparoChoque j ticks):t) = True
houveChoques (_:t) = houveChoques t

-- | Identifica os 'Jogador'es que usaram o 'Choque'.
jogadorChoque :: [Jogador] -> [Disparo] -> [Jogador]
jogadorChoque js [] = []
jogadorChoque js ((DisparoCanhao jD pD dD):t) = jogadorChoque js t
jogadorChoque js ((DisparoLaser jD pD dD):t) = jogadorChoque js t
jogadorChoque js ((DisparoChoque j ticks):t) = (selecionaJogador j js):jogadorChoque js t

-- | Identifica a área 6x6 que o 'Choque' afeta, em volta do 'Jogador' que o ativou.
areaChoque :: [PosicaoGrelha] -> [PosicaoGrelha]
areaChoque [] = []
areaChoque ((x,y):t) = [(x-3,y-3),(x-3,y-2),(x-3,y-1),(x-3,y),(x-3,y+1),(x-3,y+2),(x-3,y+3),
                        (x-2,y-3),(x-2,y-2),(x-2,y-1),(x-2,y),(x-2,y+1),(x-2,y+2),(x-2,y+3),
                        (x-1,y-3),(x-1,y-2),(x-1,y+2),(x-1,y+3),
                        (x,y-3),(x,y-2),(x,y+2),(x,y+3),
                        (x+1,y-3),(x+1,y-2),(x+1,y+2),(x+1,y+3),
                        (x+2,y-3),(x+2,y-2),(x+2,y-1),(x+2,y),(x+2,y+1),(x+2,y+2),(x+2,y+3),
                        (x+3,y-3),(x+3,y-2),(x+3,y-1),(x+3,y),(x+3,y+1),(x+3,y+2),(x+3,y+3)
                       ] ++ areaChoque t