-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2018li1g098 where

import LI11819
import Data.List
import Tarefa0_2018li1g098
import Tarefa1_2018li1g098
import Tarefa2_2018li1g098
-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (4,2), direcaoJogador = D, vidasJogador = 3, lasersJogador = 3, choquesJogador = 2},Jogador {posicaoJogador = (2,4), direcaoJogador = B, vidasJogador = 1, lasersJogador = 2, choquesJogador = 3}], disparosEstado = [DisparoCanhao {jogadorDisparo = 1, posicaoDisparo = (4,4), direcaoDisparo = B},DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (2,2), direcaoDisparo = E},DisparoChoque {jogadorDisparo = 1, tempoDisparo = 3}]}]

-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -> String
comprime (Estado map js ds) = (registaMap map) ++ "+" ++ (registaJogadores js) ++ "*" ++ (registaDisparos ds)

-- | Regista o estado do Mapa numa string
registaMap :: Mapa -> String
registaMap [[]] = ""
registaMap [h] = comprimeLinha h 
registaMap (h:t) = (comprimeLinha h) ++ "-" ++ (registaMap t)

-- | Regista uma lista de 'Peca' numa string 
comprimeLinha :: [Peca] -> String
comprimeLinha [] = [] 
comprimeLinha (h:t) | (h == (Bloco Destrutivel)) = "D" ++ (comprimeLinha t)
                    | (h == (Bloco Indestrutivel)) = "I" ++ (comprimeLinha t)
                    | (h == (Vazia)) = "V" ++ (comprimeLinha t)

-- | Regista uma lista de Jogador numa string
registaJogadores :: [Jogador] -> String
registaJogadores [] = "" 
registaJogadores (h:t) = (registaJogador h) ++ (registaJogadores t) 

-- | Regista um jogador numa string

registaJogador :: Jogador -> String
registaJogador (Jogador (x,y) d v l c) = (show x) ++ "," ++ (show y) ++ (show d) ++ (show v) ++ "|" ++ (show l) ++ ":" ++ (show c)

-- | Regista uma lista de Disparo numa string

registaDisparos :: [Disparo] -> String
registaDisparos [DisparoCanhao j (x,y) dd] = "C" ++ (show j) ++ (show x) ++ "," ++ (show y) ++ (show dd)
registaDisparos [DisparoLaser j (x,y) dd] = "L" ++ (show j) ++ (show x) ++ "," ++ (show y) ++ (show dd)
registaDisparos [DisparoChoque j ti] = "X" ++ (show j) ++ (show ti) 
registaDisparos ((DisparoCanhao j (x,y) dd) : t) = "C" ++ (show j) ++ (show x) ++ "," ++ (show y) ++ (show dd) ++ "/" ++ (registaDisparos t)
registaDisparos ((DisparoLaser j (x,y) dd) : t) = "L" ++ (show j) ++ (show x) ++ "," ++ (show y) ++ (show dd) ++ "/" ++ (registaDisparos t)
registaDisparos ((DisparoChoque j ti) : t) = "X" ++ (show j) ++ (show ti) ++ "/" ++ (registaDisparos t)  

-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime s  = Estado (descomprimeMapa s) (descomprimeListaJogadores s) (descomprimeListaDisparos s)
-- | Separa a informação relevante do mapa numa string
separarMapa :: String -> String
separarMapa [] = []
separarMapa ('+':t) = ['+']
separarMapa (h:t) = h : (separarMapa t)

-- | Separa as linhas do mapa numa string
separarLinhas :: String -> String
separarLinhas [] = []
separarLinhas (h:'+':[]) = [h]
separarLinhas (h:'-':t) = [h] 
separarLinhas (h:t) = h : (separarLinhas t)

-- | Transforma a string com a informação das linhas numa data 'Peca'
descomprimeLinhas :: String -> [Peca]
descomprimeLinhas [] = []
descomprimeLinhas [h] | h =='D' = [Bloco Destrutivel]
                      | h =='I' = [Bloco Indestrutivel]
                      | h =='V' = [Vazia]
descomprimeLinhas (h:t) | (h=='D') = (Bloco Destrutivel) : descomprimeLinhas t
                        | (h=='I') = (Bloco Indestrutivel) : descomprimeLinhas t
                        | (h=='V') = (Vazia) : descomprimeLinhas t       

-- | Transforma a string com a informação do mapa numa data 'Mapa'
descomprimeMapa :: String -> Mapa
descomprimeMapa [] = []
descomprimeMapa s = descomprimeLinhas (separarLinhas (separarMapa s)) : descomprimeMapa (drop (length (descomprimeLinhas (separarLinhas (separarMapa s))) +1) (s))

-- | Separa a informação sobre os jogadores numa string
separarListaJogadores :: String -> String
separarListaJogadores (h:i:t) | ((h == '+') && (i/='*')) = i : (separarListaJogadores (h:t))
                              | ((h == '+') && (i=='*')) = []
                              | otherwise = (separarListaJogadores (i:t))

-- | Separa a informação de cada jogador numa string individual
separarJogadores :: String -> String
separarJogadores [] = []
separarJogadores (h:t) | (h == '/') = []
                       | otherwise = h : (separarJogadores t)

-- | Tranforma uma string com a informação de um jogador numa da 'Jogador'
descomprimeJogadores :: String -> Jogador
descomprimeJogadores l = Jogador (read (descobrirX l), read (descobrirY l)) (read (descobrirDir l)) (read (descobrirVida l)) (read (descobrirLaser l)) (read (descobrirChoque l))

-- | Tranforma uma string com a informação dos jogadores numa data '[Jogador]'
descomprimeListaJogadores :: String -> [Jogador]
descomprimeListaJogadores s = (descomprimeJogadores (separarJogadores (separarListaJogadores s))) : (descomprimeListaJogadores (drop (length (separarJogadores (separarListaJogadores s)) +1) (s)))

-- | Encontra a informação da coordenada x de um jogador
descobrirX :: String -> String 
descobrirX [] = []
descobrirX (h:t) | (h==',') = []
                 | otherwise = h : descobrirX t
-- | Encontra a informação da coordenada y de um jogador
descobrirY :: String -> String
descobrirY [] = []
descobrirY (h:i:t) | ((h==',') && (i/='C' || i/='D' || i/='B'|| i/='E')) = i : descobrirY (h:t)
                   | ((h==',') && (i=='C' || i=='D' || i=='B'|| i=='E')) = []
                   | otherwise = descobrirY (i:t)

-- | Encontra a informação da direçao de um jogador
descobrirDir :: String -> String
descobrirDir [] = []
descobrirDir (h:t) | (h == 'C') = [h]
                   | (h == 'D') = [h]
                   | (h == 'B') = [h]
                   | (h == 'E') = [h]
                   | otherwise = descobrirDir t

-- | Encontra a informação das vidas de um jogador
descobrirVida :: String -> String
descobrirVida [] = []
descobrirVida (h:i:t) | ((h=='C' || h=='D' || h=='B' || h=='E') && (i/='|')) = i : descobrirVida (h:t)
                      | ((h=='C' || h=='D' || h=='B' || h=='E') && (i=='|')) = []
                      | otherwise = descobrirVida (i:t)
-- | Encontra a informação das munições laser de um jogador
descobrirLaser :: String -> String
descobrirLaser [] = []
descobrirLaser (h:i:t) | ((h == '|') && (i /= ':')) = i : descobrirLaser (h:t)
                       | ((h == '|') && (i == ':')) = []
                       | otherwise  = descobrirLaser (i:t)
-- | Encontra a informação das munições choque de um jogador
descobrirChoque :: String -> String
descobrirChoque [] = []
descobrirChoque (h:t) | (h == ':') = t
                      | otherwise = descobrirChoque t

-- | Separa a a informação de cada disparo individual numa string
separarListaDisparos :: String -> String 
separarListaDisparos [] = []
separarListaDisparos (h:t) | (h == '*') = t
                           | otherwise = separarListaDisparos t

-- | Separa a informação dos Disparos numa string
separarDisparos :: String -> String 
separarDisparos [] = []
separarDisparos (h:t) | (h == '/') = []
                      | otherwise = separarDisparos t

-- | Transforma uma string com informação de um disparo numa data 'Disparo'
descomprimeDisparos :: String -> Disparo
descomprimeDisparos ('C':h:t) = DisparoCanhao (read [h]) ((read (descobrirX t)), (read (descobrirY t))) (read (descobrirDir t)) 
descomprimeDisparos ('L':h:t) = DisparoLaser (read [h]) (read (descobrirX t), read (descobrirY t)) (read (descobrirDir t)) 
descomprimeDisparos ('X':h:t) = DisparoChoque (read [h]) (read t)

-- | Tranforma uma string com informação dos disparos numa data '[Disparo]'
descomprimeListaDisparos :: String -> [Disparo]
descomprimeListaDisparos s = (descomprimeDisparos (separarDisparos (separarListaDisparos s))) : (descomprimeListaDisparos (separarDisparos (drop (length (separarDisparos (separarListaDisparos s)) +1) (separarListaDisparos s))))
