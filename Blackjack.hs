-- fc58214, fc59... --

module Blackjack where
import BaralhosExemplo
import Data.List
data Baralho =  Baralho {cartas::[String]}
    deriving (Show, Eq, Ord)

converte :: [String] -> Baralho
converte xs = Baralho xs

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
                              maoJogador :: Baralho,
                              maoCasa :: Baralho,
                              deckAtual :: Baralho,
                              creditosAtuais :: Int,
                              descricao :: String}  


data Jogada = Stand | Hit


--falta perceber estes construtores
sempreStand :: Estrategia
sempreStand = Estrategia {aposta = 5,
                          jogada = Stand,
                          maoJogador = []{-alterar-},
                          maoCasa = []{-alterar-},
                          deckAtual = []{-alterar-},
                          creditosAtuais = creditosAtuais - aposta
                          descricao = "apostar sempre 5 créditos, fazer sempre stand"}


--falta perceber estes construtores
sempreHit :: Estrategia
sempreHit = Estrategia {aposta = 5,
                          jogada = Hit,
                          maoJogador = []{-alterar-},
                          maoCasa = []{-alterar-},
                          deckAtual = []{-alterar-},
                          creditosAtuais = creditosAtuais - aposta,
                          descricao = "apostar sempre 5 créditos, fazer sempre hit"}

--perceber e alterar
estrategia3 :: Estrategia
estrategia3 = {aposta = 5,
               jogada = Stand,
               maoJogador = []{-alterar-},
               maoCasa = []{-alterar-},
               deckAtual = []{-alterar-},
               creditosAtuais = creditosAtuais - aposta,
               descricao = "a definir"}

{-simula uma ronda do jogo Blackjack, utilizando a estratégia dada. A função
devolve o estado de jogo no final da ronda (atualizando o valor dos créditos e
do baralho).
-}
simulaJogo :: Estrategia -> EstadoJogo -> EstadoJogo
simulaJogo strat state = 

instance Show Estrategia where 
    show s = "Estrategia: " ++ descricao s

