-- fc58214, fc59... --

module BjDiscard where
import BaralhosExemplo
import Data.List
import Data.Char


--passar para type Baralho?--
type Baralho = [String]


converte :: [String] -> Baralho
converte xs = id xs

tamanho :: Baralho -> Int
tamanho deck = foldr(\x acc -> acc+1) 0 deck

data EstadoJogo = EstadoJogo{ curDeck :: Baralho,
                              creds :: Int,
                              cartasJogador :: Baralho,
                              cartasCasa :: Baralho,
                              rondaTerminada :: Bool }
    deriving (Eq, Ord)

inicializa :: Baralho -> EstadoJogo
inicializa deck = EstadoJogo{curDeck = deck,
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

rondaTerm :: EstadoJogo -> Bool
rondaTerm state = tamanho (curDeck (state)) <= 20 || valoresMao (cartasJogador (state)) >= 21 ||
                     valoresMao (cartasCasa (state)) >= 21

instance Show EstadoJogo where
    show s = "jogador: " ++ Prelude.show (cartasJogador s) ++ "\ncasa: " ++ Prelude.show (cartasCasa s) ++ "\ncreditos: " ++ Prelude.show(creds s)
        --unlines ["jogador: " ++  Prelude.show (cartasJogador s), "casa: " ++ Prelude.show (cartasCasa s) , "creditos: " ++ Prelude.show(creds s) ]
        --intercalate "\n" ["jogador: " ++ Prelude.show (cartasJogador s), "casa: " ++ Prelude.show (cartasCasa s), "creditos: " ++ Prelude.show (creds s)]

-- mudar argumentos? 
data Estrategia = Estrategia {aposta :: Int,
                              jogada :: [Jogada],
                              descricao :: String}  

data Jogada = Stand | Hit
    deriving (Eq)


--falta perceber estes construtores
sempreStand :: Estrategia
sempreStand = Estrategia {aposta = 5,
                          jogada = repeat Stand,
                          descricao = "apostar sempre 5 creditos, fazer sempre stand"}


--falta perceber estes construtores
sempreHit :: Estrategia
sempreHit = Estrategia {aposta = 5,
                          jogada = repeat Hit,
                          descricao = "apostar sempre 5 creditos, fazer sempre hit"}

--perceber e alterar
estrategia3 :: Estrategia
estrategia3 = Estrategia{aposta = 5,
               jogada = [Stand],
               descricao = "a definir"}

{-simula uma ronda do jogo Blackjack, utilizando a estratégia dada. A função
devolve o estado de jogo no final da ronda (atualizando o valor dos créditos e
do baralho).
-}
simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
simulaRonda (Estrategia a js desc) (EstadoJogo d c cj cc t) = if rondaTerm estadoInicial then estadoInicial else estadoInicial
    where 
        estadoInicial = (EstadoJogo (drop 4 d) (c - a) (take 2 d) (take 2 (drop 2 d)) (rondaTerm estadoInicial))
        


jogadaHit :: EstadoJogo -> EstadoJogo
jogadaHit (EstadoJogo d c cj cc t) =  if tamanho (tail d) > 0 
                                                            then (EstadoJogo (tail d) c (cj ++ [head d]) cc (tamanho d<=20 || valoresMao (cj ++ [head d]) >= 21 || valoresMao cc >= 21))
                                                            else (EstadoJogo d c cj cc True)

jogadaStand :: EstadoJogo -> EstadoJogo  
jogadaStand (EstadoJogo d c cj cc t) = if tamanho (tail d) > 0
                                                             then (EstadoJogo (tail d) c cj (cc ++ [head d]) (tamanho d <= 20 || valoresMao cj >= 21 || valoresMao (cc ++ [head d]) >= 21))                                               
                                                             else (EstadoJogo d c cj cc (tamanho d <= 20 || valoresMao cj >= 21 || valoresMao (cc ++ [head d]) >= 21))

{--if 
case (head js) of Hit -> simulaRonda (Estrategia a (tail js) desc) (jogadaHit estadoInicial)
                              Stand -> simulaRonda (Estrategia a (tail js) desc) (jogadaStand estadoInicial)
--}



playByPlay :: Estrategia -> EstadoJogo -> EstadoJogo -> EstadoJogo
playByPlay (Estrategia aposta jogada descricao) initState state2 = 
                                    if (head jogada == Hit) && (not $ rondaTerm initState) && (not $ rondaTerm state2)
                                    then (playByPlay (Estrategia aposta (tail jogada) descricao) state2 playerHits)  
                                    else (playByPlay (Estrategia aposta (tail jogada) descricao) state2 houseHits)
    where   
        playerHits = EstadoJogo{curDeck = tail (baralho initState),
                                    creds = creds initState,
                                    cartasJogador = (cartasJogador initState)++ [head (baralho initState)],
                                    cartasCasa = cartasCasa initState,
                                    rondaTerminada = rondaTerm playerHits || tamanho (curDeck playerHits) <= 20 
                                    || valoresMao (cartasJogador playerHits) >= 21 || valoresMao (cartasCasa playerHits) >= 21}

        houseHits = EstadoJogo{curDeck = tail (baralho initState),
                                    creds = creds initState,
                                    cartasJogador = cartasJogador initState,
                                    cartasCasa = (cartasCasa initState) ++ [head (baralho initState)],
                                    rondaTerminada = rondaTerm houseHits || tamanho (curDeck houseHits) <= 20 
                                    || valoresMao (cartasJogador houseHits) >= 21 || valoresMao (cartasCasa houseHits) >= 21}         
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





data Result = W | D | L