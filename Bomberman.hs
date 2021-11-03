module Bomberman where

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- TIPIFICACAO DO JOGO

data Item = JogadorX | JogadorY | JogadorW | JogadorZ | Grama | Patins | Arremesso | Fogo | Bomba | Parede | Pedra deriving (Eq, Show) -- Parede eh o destrutivel
data Orientacao = N | S | L | O deriving (Eq, Show)
data Identificador = X | Y | W | Z deriving (Eq, Show)
type Jogador = (Identificador, (Int, Int), Orientacao, ((Item, Int), (Item, Int), (Item, Int)))
type Instante = (Tabuleiro, [Jogador])
type Celula = [Item]
type Linha = [Celula]
type Tabuleiro = [Linha]

------------------------------------------------------------------------------
-- VERIFICA VALIDADE DE UMA CELULA

buraco :: Celula -> Bool
buraco [] = True
buraco _ = False

pertence :: Item -> Celula -> Bool
pertence _ [] = False 
pertence y (x:xs) = x == y || pertence y xs

unicidade :: Celula -> Bool
unicidade [_] = True
unicidade [] = True
unicidade (x:xs)
   | x `pertence` xs = False
   | otherwise = unicidade xs

presente :: Item -> Bool
presente p = p == Patins || p == Arremesso || p == Fogo

sobreposicoes :: Celula -> Bool
sobreposicoes [] = True
sobreposicoes [x] = x == Grama || x == Pedra || x == Parede
sobreposicoes (x:xs)
   | x == Grama || x == Pedra = null xs
   | x == Bomba || jogador x || presente x || x == Parede = y == Grama && sobreposicoes xs
   | otherwise = y == Grama || presente y
   where
      y = head xs 

celulaValida :: Celula -> Bool
celulaValida [] = True -- buraco
celulaValida cel = unicidade cel && sobreposicoes cel

------------------------------------------------------------------------------
-- CRIACAO DE UM TABULEIRO VALIDO

-- CRIA SE TABULEIRO VALIDO E RETORNA LISTA DE JOGADORES
criaTabuleiro :: Tabuleiro -> Instante
criaTabuleiro tab
   | validadeTabuleiro = (tab, jogadores)
   | otherwise = error "Tabuleiro invalido!"
   where
      validadeTabuleiro = dimensoesValidas tab && tabuleiroValido tab
      jogadores = setaJogadores tab 1

linhaValida :: Linha -> Bool
linhaValida [] = True
linhaValida (x:xs)
   | celulaValida x = linhaValida xs
   | otherwise = False

tabuleiroValido :: Tabuleiro -> Bool
tabuleiroValido [] = True
tabuleiroValido (x:xs)
   | linhaValida x = tabuleiroValido xs
   | otherwise = False

-- CONSIDEREI TABULEIRO VALIDO SSE QTD DE COLUNAS >= 3 (E TABULEIRO RETANGULAR)
dimensaoLinhas :: Tabuleiro -> Bool
dimensaoLinhas [x, y] = length x == length y && length x >= 3
dimensaoLinhas (x:y:xs) = length x == length y && dimensaoLinhas (y:xs)

-- CONSIDEREI TABULEIRO VALIDO SSE QTD DE LINHAS >= 3
dimensoesValidas :: Tabuleiro -> Bool
dimensoesValidas tab = length tab >= 3  && dimensaoLinhas tab

-- BUSCA NO TABULEIRO SE HA JOGADORES E CRIA UMA LISTA DE JOGADORES (setados com 1 em cada na mochila)
setaJogadores :: Tabuleiro -> Int -> [Jogador]
setaJogadores [] _ = []
setaJogadores (l:ls) vl = parcial ++ setaJogadores ls (vl + 1)
   where
      parcial = setaJogadores' l vl 1

setaJogadores' :: Linha -> Int -> Int -> [Jogador]
setaJogadores' [] _ _ = []
setaJogadores' (c:cs) vl vc
   | buraco c = setaJogadores' cs vl (vc + 1)
   | not (jogador topo) = setaJogadores' cs vl (vc + 1)
   | otherwise = (jogadorIdentificador topo, (vl, vc), N, ((Patins, 1), (Fogo, 1), (Arremesso, 1))) : setaJogadores' cs vl (vc + 1)
   where
      (topo:_) = c

------------------------------------------------------------------------------
-- IMPLEMENTACOES PARA JOGADOR

-- PEGA ITEM (JOGADOR_X,Y,W,Z) EQUIVALENTE AO IDENTIFICADOR (Identificador id=X,Y,W,Z)
jogadorItem :: Identificador -> Item
jogadorItem id
   | id == X = JogadorX
   | id == Y = JogadorY
   | id == W = JogadorW
   | otherwise = JogadorZ

jogadorIdentificador :: Item -> Identificador
jogadorIdentificador item
   | item == JogadorX = X
   | item == JogadorY = Y
   | item == JogadorW = W
   | otherwise = Z

jogador :: Item -> Bool 
jogador j = j == JogadorX || j == JogadorY || j == JogadorW || j == JogadorZ

------------
-- VE SE UM JOGADOR ESPECIFICO ESTAH EM UMA LINHA
jogadorXPresenteLinha :: Linha -> Identificador -> Bool
jogadorXPresenteLinha [] _ = False
jogadorXPresenteLinha (c:cs) id
   | buraco c = jogadorPresenteLinha cs
   | otherwise = topo == jogadorItem id || jogadorXPresenteLinha cs id
   where
      (topo:_) = c

temJogadorX :: Tabuleiro -> Identificador -> Bool
temJogadorX [] _ = False
temJogadorX (l:ls) id = jogadorXPresenteLinha l id || temJogadorX ls id

listaJogadorX :: [Jogador] -> Identificador -> Jogador
listaJogadorX [] _ = error "nao ha jogador com esse identificador na lista de jogadores"
listaJogadorX (j@(id, p, o, m):js) ident
   | id == ident = j
   | otherwise = listaJogadorX js ident

------------
-- QUALQUER JOGADOR

jogadorPresenteLinha :: Linha -> Bool
jogadorPresenteLinha [] = False
jogadorPresenteLinha (c:cs)
   | buraco c = jogadorPresenteLinha cs
   | otherwise = jogador topo || jogadorPresenteLinha cs
   where
      (topo:_) = c

temJogador :: Tabuleiro -> Bool
temJogador [] = False
temJogador (l:ls) = jogadorPresenteLinha l || temJogador ls

atualizaJogadores :: [Jogador] -> Jogador -> [Jogador]
atualizaJogadores [] _ = []
atualizaJogadores (j@(id, p, oient, m):js) jog@(ident, px, ox, mx)
   | id == ident = jog:js
   | otherwise = j : atualizaJogadores js jog

--------------------

-- BUSCA TODOS JOGADORES DO TAB - RETORNA UMA LISTA DOS ITENS DELES
buscaJogadores :: Tabuleiro -> [Item]
buscaJogadores tab = buscaJogadores' tab 1

buscaJogadores' :: Tabuleiro -> Int -> [Item]
buscaJogadores' [] _ = []
buscaJogadores' (l:ls) vl = parcial ++ buscaJogadores' ls (vl + 1)
   where
      parcial = buscaLinha l vl 1

buscaLinha :: Linha -> Int -> Int -> [Item]
buscaLinha [] _ _ = []
buscaLinha (c:cs) vl vc
   | buraco c = buscaLinha cs vl (vc + 1)
   | not (jogador topo) = buscaLinha cs vl (vc + 1)
   | otherwise = topo : buscaLinha cs vl (vc + 1)
   where
      (topo:_) = c


------------------------------------------------------------------------------
-- IMPLEMENTACOES DE VIZINHO

-- RETORNA CELULA DO VIZINHO E SUA POSICAO
vizinho :: Tabuleiro -> Int -> Int -> Orientacao -> (Celula, (Int, Int))
vizinho tab l c mov
   | movimentoInvalido l c tab mov = ([], (0,0))
   | mov == N = (tab !! (l-2) !! (c-1), (l-1, c))
   | mov == S = (tab !! l !! (c-1), (l+1, c))
   | mov == L = (tab !! (l-1) !! c, (l, c+1))
   | otherwise = (tab !! (l-1) !! (c-2), (l, c-1))

vizinhoValido ::  Int -> Int -> Bool 
vizinhoValido 0 0 = False 
vizinhoValido _ _ = True

------------------------------------------------------------------------------
-- ATUALIZACAO DE TABULEIRO (MOVIMENTANDO UM JOGADOR)

-- ATUALIZA O STATUS DO GAME DADO A MOVIMENTACAO DE UM JOGADOR (SE JOGADOR NAO EXISTE NO TABULEIRO, VAZ NADA, OU SEJA, RETORNA O MESMO STATUS DE GAME)
movimento :: Instante -> Identificador -> Orientacao -> Instante
movimento (tab, jogs) id orient
   | temJogadorX tab id = (novotab, atualizaJogadoresMovimento jogs novojog)
   | otherwise = (tab, jogs)
      where
         (novotab, novojog) = movimenta (tab, listaJogadorX jogs id) orient

-- MOVIMENTA UM JOGADOR E ATUALIZA SEU STATUS
movimenta :: (Tabuleiro, Jogador) -> Orientacao -> (Tabuleiro, Jogador)
movimenta (tab, (id, (l, c), orient, ((Patins, x), (Fogo, y), (Arremesso, z)))) mov
   | movimentoInvalido l c tab mov = (tab, (id, (l, c), mov, ((Patins, x), (Fogo, y), (Arremesso, z)))) -- movimento invalido
   | buraco viz = (atualizaTab tab l c as, (id, (0, 0), mov, ((Patins, x), (Fogo, y), (Arremesso, z)))) --sem jogador, morreu, caiu no buraco
   | v == Pedra || v == Parede || jogador v || v == Bomba = (tab, (id, (l, c), mov, ((Patins, x), (Fogo, y), (Arremesso, z)))) -- nao muda tabuleiro
   | v == Patins = (atualizaTab (atualizaTab tab l c as) nl nc (itemJogador:vs), (id, (nl, nc), mov, ((Patins, x+1), (Fogo, y), (Arremesso, z)))) --jogador ganha presente
   | v == Arremesso = (atualizaTab (atualizaTab tab l c as) nl nc (itemJogador:vs), (id, (nl, nc), mov, ((Patins, x), (Fogo, y), (Arremesso, z+1)))) --jogador ganha presente
   | v == Fogo = (atualizaTab (atualizaTab tab l c as) nl nc (itemJogador:vs), (id, (nl, nc), mov, ((Patins, x), (Fogo, y+1), (Arremesso, z)))) --jogador ganha presente
   | otherwise = (atualizaTab (atualizaTab tab l c as) nl nc (itemJogador:v:vs), (id, (nl, nc), mov, ((Patins, x), (Fogo, y), (Arremesso, z))))  --grama
   where   
      (a:as) = celula tab l c -- a de atual... celula do jogador
      itemJogador = jogadorItem id
      (viz, (nl, nc)) = vizinho tab l c mov
      (v:vs) = viz

atualizaJogadoresMovimento :: [Jogador] -> Jogador -> [Jogador]
atualizaJogadoresMovimento [] _ = []
atualizaJogadoresMovimento (j@(id, p, o, m):js) jog@(nid, np, no, nm)
   | id == nid = if np == (0,0) then js else jog : js
   | otherwise = j : atualizaJogadoresMovimento js jog

-- VERIFICA SE EH UM MOVIMENTO (MOVIMENTANDO PRA BORDA) INVALIDO
movimentoInvalido :: Int -> Int -> Tabuleiro -> Orientacao -> Bool
movimentoInvalido l c (v:vs) mov = (l == 1 && mov == N) || (c == 1 && mov == O) || (l == qtdLinhas && mov == S) || (c == qtdColunas && mov == L)
   where
      qtdLinhas = length (v:vs)
      qtdColunas = length v

------------------------------------------------------------------------------
-- ATUALIZACAO DE TABULEIRO

-- ATUALIZA UMA UNICA CELULA DADO LINHA E COLUNA E CELULA E RETORNA TABULEIRO
atualizaTab :: Tabuleiro -> Int -> Int -> Celula -> Tabuleiro
atualizaTab tab l c cel = take (l-1) tab ++ [linhaAtualizada] ++ drop l tab
   where
      linhaAtualizada = atualizaCelula (tab !! (l-1)) c cel

-- ATUALIZA UMA CELULA EM UMA LINHA E RETORNA LINHA
atualizaCelula :: Linha -> Int -> Celula -> Linha
atualizaCelula linha c atual
   | not (celulaValida atual) = error "atualizacao de celula invalida!"
   | otherwise = take (c-1) linha ++ [atual] ++ drop c linha

-- ATUALIZA UMA LINHA COMPLETA EM UM TABULEIRO E RETORNA TABULEIRO
atualizaLinha :: Tabuleiro -> Linha -> Int -> Tabuleiro
atualizaLinha tab atual l
   | linhaValida atual = take (l-1) tab ++ [atual] ++ drop l tab
   | otherwise = error "atualizacao de linha invalida!"

-- ATUALIZA UMA COLUNA COMPLETA EM UM TABULEIRO E RETORNA TABULEIRO
atualizaColuna :: Tabuleiro -> Linha -> Int -> Tabuleiro
atualizaColuna [] _ vc = []
atualizaColuna (l:ls) (c:cs) vc = atualizaCelula l vc c : atualizaColuna ls cs vc

------------------------------------------------------------------------------
-- BUSCA EM TABULEIRO

nthColuna :: Tabuleiro -> Int -> Linha
nthColuna [] _ = []
nthColuna (l:ls) c = l !! (c-1) : nthColuna ls c

nthLinha :: Tabuleiro -> Int -> Linha
nthLinha [] _ = []
nthLinha tab l = tab !! (l-1)

celula :: Tabuleiro -> Int -> Int -> Celula
celula tab l c
   | length tab < l = error "posicao invalida"
   | otherwise = tab !! (l-1) !! (c-1)

------------------------------------------------------------------------------
-- COLOCA - coloca uma bomba em uma posicao vizinha (soh pode assim pois por especificacao um jogador nao pode ficar nunca na mesma celula que uma bomba)

coloca :: Instante -> Identificador -> Orientacao -> Instante
coloca (tab, jogs) id orient
   | not (temJogadorX tab id) || movimentoInvalido l c tab orient  || buraco viz = (tab, novojogs)
   | v /= Grama = (tab, novojogs)
   | otherwise = (atualizaTab tab vl vc (Bomba:viz), novojogs)
   where
      jog@(ident, (l, c), o, m) = listaJogadorX jogs id
      (viz, (vl, vc)) = vizinho tab l c orient
      (v:vs) = viz
      novojogs = atualizaJogadores jogs (ident, (l, c), orient, m)
   
------------------------------------------------------------------------------
-- ARREMESSO - arremessa soh se tiver buraco ou grama como vizinhos da bomba... se cair no buraco, cai no limbo

arremesso :: Instante -> Identificador -> Orientacao -> Instante
arremesso (tab, jogs) id orient
   | not (temJogadorX tab id) || o /= orient = (tab, jogs)
   | otherwise = (arremessa tab jog, jogs)
   where
      jog@(ident, p, o, m) = listaJogadorX jogs id

arremessa :: Tabuleiro -> Jogador -> Tabuleiro
arremessa tab (id, (l, c), orient, (_, _, (_, potencia)))
   | movimentoInvalido l c tab orient || buraco viz || potencia == 0 = tab
   | v /= Bomba = tab
   | vl == ul && vc == uc = tab
   | buraco ult = atualizaTab tab vl vc vs
   | otherwise = atualizaTab (atualizaTab tab vl vc vs) ul uc (v:ult)
   where
      (viz, (vl, vc)) = vizinho tab l c orient
      (v:vs) = viz
      (ult, (ul, uc)) = ultimaCelulaValida tab vl vc orient potencia -- pega linha e coluna da ultima celula onde a bomba poderia cair - potencia, se comeca a contar de onde esta bomba... (potencia-1) se comeca a contar de onde esta jogador

-- valido enquanto estiver grama ou vazio
ultimaCelulaValida :: Tabuleiro -> Int -> Int -> Orientacao -> Int -> (Celula, (Int, Int))
ultimaCelulaValida tab l c mov potencia
   | movimentoInvalido vl vc tab mov || potencia == 0 = (celula tab l c, (l, c))
   | buraco viz = ultimaCelulaValida tab vl vc mov (potencia-1)
   | v == Grama = ultimaCelulaValida tab vl vc mov (potencia-1)
   | otherwise = (celula tab l c, (l, c))
   where
      (viz, (vl, vc)) = vizinho tab l c mov
      (v:vs) = viz

------------------------------------------------------------------------------
-- EXPLOSAO

{-
(1) - PARA BOMBA E NAO DESTROI = Buraco, Pedra, Bomba, chegou no final
(2) - PARA BOMBA E DESTROI = Jogador, Parede
(3) - NAO PARA BOMBA E DESTROI = Presente
(4) - NAO PARA BOMBA E NAO DESTROI = Grama

*Parede eh destrutivel, Pedra eh INdestrutivel
*Se bate no jogador, parar a sequenca de explosao (outro jogador pode se esconder atras de um jogador que sera explodido)
-}

explosao :: Instante -> Int -> Int -> Int -> Instante
explosao (tab, jogs) l c intensidade = (novotab, novojogs)
   where
      novotab = explode tab l c intensidade
      novojogs = atualizaJogadoresExplosao novotab jogs

atualizaJogadoresExplosao :: Tabuleiro -> [Jogador] -> [Jogador]
atualizaJogadoresExplosao _ [] = []
atualizaJogadoresExplosao tab (j@(id, _, _, _):js)
   | temJogadorX tab id = j : atualizaJogadoresExplosao tab js
   | otherwise = atualizaJogadoresExplosao tab js

-- ATUALIZA TABULEIRO DADO A EXPLOSAO DE UMA BOMBA (l,c) DE FORCA intensidade
explode :: Tabuleiro -> Int -> Int -> Int -> Tabuleiro
explode tab l c intensidade
   | buraco bomba = tab
   | b /= Bomba = if head bs == Bomba then atualizaTab explodido l c (tail bs) else tab
   | otherwise = atualizaTab explodido l c bs -- tira a bomba da celula
   where
      bomba = celula tab l c
      (b:bs) = bomba
      explodido = explode' tab l c intensidade

explode' :: Tabuleiro -> Int -> Int -> Int -> Tabuleiro
explode' tab l c intensidade = explodeColuna (explodeLinha tab l c intensidade) l c intensidade

explodeLinha :: Tabuleiro -> Int -> Int -> Int -> Tabuleiro
explodeLinha tab l c intensidade = atualizaLinha tab (explodeLinhaOrientacional (explodeLinhaOrientacional (nthLinha tab l) l c intensidade L) l c intensidade O) l

-- l c sao posicao da bomba... vai explodindo a bomba na linha em direcao ao orient
explodeLinhaOrientacional :: Linha -> Int -> Int -> Int -> Orientacao -> Linha
explodeLinhaOrientacional linha l c intensidade orient
   | intensidade == 0 || buraco viz || not (vizinhoValido vl vc) = linha
   | v == Pedra || v == Bomba = linha -- (1)
   | v == Parede || jogador v = atualizaCelula linha vc vs -- (2)
   | presente v = explodeLinhaOrientacional (atualizaCelula linha vc vs) l novaCol (intensidade-1) orient -- (3)
   | otherwise = explodeLinhaOrientacional linha l novaCol (intensidade-1) orient -- (4)
   where
      novaCol = if orient == L then c+1 else c-1
      (viz, (vl, vc)) = vizinho [linha] 1 c orient
      (v:vs) = viz

explodeColuna :: Tabuleiro -> Int -> Int -> Int -> Tabuleiro
explodeColuna tab l c intensidade = explodeColunaOrientacional (explodeColunaOrientacional tab l c intensidade N) l c intensidade S

-- l c sao posicao da bomba... vai explodindo a bomba na coluna em direcao ao orient
explodeColunaOrientacional :: Tabuleiro -> Int -> Int -> Int -> Orientacao -> Tabuleiro
explodeColunaOrientacional tab l c intensidade orient
   | intensidade == 0 || buraco viz || not (vizinhoValido vl vc) = tab
   | v == Pedra || v == Bomba = tab -- (1)
   | v == Parede || jogador v = atualizaTab tab vl vc vs -- (2)
   | presente v = explodeColunaOrientacional (atualizaTab tab vl vc vs) novaLin c (intensidade-1) orient -- (3)
   | otherwise = explodeColunaOrientacional tab novaLin c (intensidade-1) orient -- (4)
   where
      novaLin = if orient == S then l+1 else l-1
      (viz, (vl, vc)) = vizinho tab l c orient
      (v:vs) = viz

------------------------------------------------------------------------------
-- FIM

-- para um jogador soh (por exemplo, jogando contra maquina, os outros jogadores sao maquina)
-- chega ao fim quando o jogador entrado nao esta mais no tabuleiro
fim :: Instante -> Identificador -> Bool
fim (tab, _) id = not (temJogadorX tab id)

-- deathmatch (acaba soh quando soh sobrou um jogador)
fim' :: Instante -> Bool 
fim' (_, jogs) = length jogs == 1

{-

ESSES SAO DADO UM TABULEIRO E UM JOGADOR (implementacao alternativa)

fim :: Tabuleiro -> Jogador -> Bool
fim tab (id, _, _, _) = not (temJogadorX tab id)

fim' :: Tabuleiro -> Bool 
fim' tab = not (temJogador tab)

-}
