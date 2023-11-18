-- fc58214, fc59... --

module Blackjack where
import BaralhosExemplo
import Data.List
import Data.Char


--passar para type Baralho?--
data Baralho =  Baralho {cartas::[String]}
    deriving (Show, Eq, Ord)

converte :: [String] -> Baralho
converte xs = Baralho (id xs)

tamanho :: Baralho -> Int
tamanho (Baralho {cartas = deck}) = foldr(\x acc -> acc+1) 0 deck

data EstadoJogo = EstadoJogo{ curDeck :: Baralho,
                              creds :: Int,
                              cartasJogador :: [Baralho],
                              cartasCasa :: [Baralho],
                              rondaTerminada :: Bool }
    deriving (Eq, Ord)

inicializa :: Baralho -> EstadoJogo
inicializa (Baralho {cartas = deck}) = EstadoJogo{curDeck = Baralho deck,
                                                  creds = 100,
                                                  cartasJogador = [],
                                                  cartasCasa = [],
                                                  rondaTerminada = False } 


creditos :: EstadoJogo -> Int
creditos e = creds e

baralho :: EstadoJogo -> Baralho
baralho b = curDeck b

terminado :: EstadoJogo -> Bool
terminado t = (tamanho (baralho t) <= 20) || (creditos t <= 0)

instance Show EstadoJogo where
    show s = "jogador: " ++ Prelude.show (cartasJogador s) ++ "\ncasa: " ++ Prelude.show (cartasCasa s) ++ "\ncreditos: " ++ Prelude.show(creds s)
        --unlines ["jogador: " ++  Prelude.show (cartasJogador s), "casa: " ++ Prelude.show (cartasCasa s) , "creditos: " ++ Prelude.show(creds s) ]
        --intercalate "\n" ["jogador: " ++ Prelude.show (cartasJogador s), "casa: " ++ Prelude.show (cartasCasa s), "creditos: " ++ Prelude.show (creds s)]

-- mudar argumentos? 
data Estrategia = Estrategia {aposta :: Int,
                              jogada :: Jogada,
                              descricao :: String}  


data Jogada = Stand | Hit


--falta perceber estes construtores
sempreStand :: Estrategia
sempreStand = Estrategia {aposta = 5,
                          jogada = Stand,
                          descricao = "apostar sempre 5 créditos, fazer sempre stand"}


--falta perceber estes construtores
sempreHit :: Estrategia
sempreHit = Estrategia {aposta = 5,
                          jogada = Hit,
                          descricao = "apostar sempre 5 créditos, fazer sempre hit"}

--perceber e alterar
estrategia3 :: Estrategia
estrategia3 = Estrategia{aposta = 5,
               jogada = Stand,
               descricao = "a definir"}

{-simula uma ronda do jogo Blackjack, utilizando a estratégia dada. A função
devolve o estado de jogo no final da ronda (atualizando o valor dos créditos e
do baralho).
-}
--simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
{--simulaRonda strat state = EstadoJogo{(curDeck = x:y:z:w:(baralho state)),
                                     creds = creditos state
                                     cartasJogador = x:y:[],
                                     cartasCasa = z:w:[],
                                     terminado = False
                                    }
--}
{-dada uma
estratégia do jogador e um baralho, corre uma simulação de um jogo
completo de Blackjack, com um saldo inicial de 100 créditos. A função
devolve o número de créditos do jogador no final da simulação.-}
--imulaJogo:: Estrategia -> Baralho -> Int
--simulaJogo strat deck = 


instance Show Estrategia where 
    show s = "Estrategia: " ++ descricao s

valoresMao :: [String]-> Int
valoresMao d = foldl(\acc x -> b x acc) 0 d
    where b x acc
            | head x == 'A' = if (acc + 11 > 21) then (acc + 1) else (acc + 11)
            | elem (head x) "23456789" =  acc + digitToInt (head x)
            | otherwise = acc + 10