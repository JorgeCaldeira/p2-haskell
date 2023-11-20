-- fc58214, fc59786 --

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
rondaTerm state = tamanho (curDeck (state)) <= 20 || valoresMao (cartasJogador (state)) > 21 ||
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
    deriving (Eq, Show)



sempreStand :: Estrategia
sempreStand = Estrategia {aposta = 5,
                          jogada = repeat Stand,
                          descricao = "apostar sempre 5 creditos, fazer sempre stand"}



sempreHit :: Estrategia
sempreHit = Estrategia {aposta = 5,
                          jogada = repeat Hit,
                          descricao = "apostar sempre 5 creditos, fazer sempre hit"}

--alterar
estrategia3 :: Estrategia
estrategia3 = Estrategia{aposta = 5,
               jogada = cycle [Hit, Stand],
               descricao = "a definir"}
    
{-simula uma ronda do jogo Blackjack, utilizando a estratégia dada. A função
devolve o estado de jogo no final da ronda (atualizando o valor dos créditos e
do baralho).
-}
simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
simulaRonda (Estrategia a js desc) (EstadoJogo (x:y:z:w:d) c cj cc t) = if rondaTerm estadoInicial 
                                                              then estadoInicial 
                                                              else escolheJogada (Estrategia a js desc) estadoInicial
    where 
        estadoInicial = (EstadoJogo d (c - a) ([x,y]) ([z,w]) (rondaTerm estadoInicial))
        
escolheJogada :: Estrategia -> EstadoJogo -> EstadoJogo
escolheJogada (Estrategia a (j:js) desc) (EstadoJogo d c cj cc t) = if t
                                                                then updateCreds (Estrategia a (j:js) desc) (EstadoJogo d c cj cc t)
                                                                else
                                                                    if (j == Hit) && (valoresMao cj /= 21)  
                                                                    then escolheJogada (Estrategia a js desc) (jogadaHit (EstadoJogo d c cj cc t))
                                                                    else escolheJogada (Estrategia a js desc) (jogadaStand (EstadoJogo d c cj cc t))
jogadaHit :: EstadoJogo -> EstadoJogo
jogadaHit (EstadoJogo d c cj cc t) =  if tamanho (tail d) > 0 
                                                            then (EstadoJogo (tail d) c (cj ++ [head d]) cc (tamanho d<=20 || valoresMao (cj ++ [head d]) > 21 || valoresMao cc >= 17))
                                                            else (EstadoJogo d c cj cc True)

jogadaStand :: EstadoJogo -> EstadoJogo  
jogadaStand (EstadoJogo d c cj cc t) = if (tamanho (tail d) > 0 && valoresMao cc < 17)
                                                             then 
                                                                (if valoresMao (cc ++ [head d]) < 17
                                                                then jogadaStand (EstadoJogo (tail d) c cj (cc ++ [head d]) (tamanho d <= 20 || valoresMao cj >= 21 || valoresMao (cc ++ [head d]) >= 17))
                                                                else  (EstadoJogo (tail d) c cj (cc ++ [head d]) (tamanho d <= 20 || valoresMao cj > 21 || valoresMao (cc ++ [head d]) >= 17))                                               
                                                             )else (EstadoJogo d c cj cc (tamanho d <= 20 || valoresMao cj > 21 || valoresMao (cc ++ [head d]) >= 17))



 {--then (EstadoJogo (tail d) c cj (cc ++ [head d]) (tamanho d <= 20 || valoresMao cj >= 21 || valoresMao (cc ++ [head d]) >= 17))                                               
                                                             else (EstadoJogo d c cj cc (tamanho d <= 20 || valoresMao cj >= 21 || valoresMao (cc ++ [head d]) >= 17))
--}
{--if 
case (head js) of Hit -> simulaRonda (Estrategia a (tail js) desc) (jogadaHit estadoInicial)
                              Stand -> simulaRonda (Estrategia a (tail js) desc) (jogadaStand estadoInicial)
--}



        
--}
{-dada uma
estratégia do jogador e um baralho, corre uma simulação de um jogo
completo de Blackjack, com um saldo inicial de 100 créditos. A função
devolve o número de créditos do jogador no final da simulação.-}
--simulaJogo:: Estrategia -> Baralho -> Int
--simulaJogo strat deck = 


instance Show Estrategia where 
    show s = "Estrategia: " ++ descricao s

{--valoresMao :: [String]-> Int
valoresMao d = foldl(\acc x -> b x acc) 0 d
    where b x acc
            | head x == 'A' = if (acc + 11 > 21) then (acc + 1) else (acc + 11)
            | elem (head x) "23456789" =  acc + digitToInt (head x)
            | otherwise = acc + 10
--}
escolheValores :: [String] -> Int
escolheValores d = if (valoresA11 d) <= 21 then valoresA11 d else valoresA1 d

valoresA1 :: [String] -> Int
valoresA1 d = foldl(\acc x -> b x acc) 0 d
    where b x acc
            | head x == 'A' = acc + 1
            | elem (head x) "23456789" =  acc + digitToInt (head x)
            | otherwise = acc + 10

valoresA11 :: [String] -> Int
valoresA11 d = foldl(\acc x -> b x acc) 0 d
    where b x acc
            | head x == 'A' =  (acc + 11)
            | elem (head x) "23456789" =  acc + digitToInt (head x)
            | otherwise = acc + 10
            
--updateCreds :: EstadoJogo -> Estrategia -> EstadoJogo
--updateCreds (Estrategia a js desc) (EstadoJogo d c cj cc t) = case x of (((valoresMao cj) < (valoresMao cc) && (valoresMao cj) <= 21) || (valoresMao cc > 21)) -> (EstadoJogo d (c+10) cj cc t)
 --                                                                       (valores cj == valoresMao cc && valoresMao <= 21) -> (EstadoJogo d (c+a) cj cc t)
   --                                                                     otherwise -> (EstadoJogo d c cj cc t)

updateCreds :: Estrategia -> EstadoJogo -> EstadoJogo
updateCreds (Estrategia a js desc) (EstadoJogo d c cj cc t) =
  case x of
    True  -> EstadoJogo d c cj cc t
    False -> case y of
      True  -> EstadoJogo d (c + a) cj cc t
      False -> EstadoJogo d (c + 2*a) cj cc t
  where
    x = (valoresMao cj < valoresMao cc && valoresMao cc <= 21)|| valoresMao cj > 21
    y = valoresMao cj == valoresMao cc && valoresMao cj <= 21

valoresMao :: [String] -> Int
valoresMao [] = 0
valoresMao (d:dx) = if (elem (head d) "23456789")
                 then (digitToInt (head d) + valoresMao dx)
                 else 
                    (if (head d == 'A')
                    then 
                        (if ((11 + valoresMao dx) <= 21)
                        then (11 + valoresMao dx)
                        else (1 + valoresMao dx)
                    )else (10 + valoresMao dx))