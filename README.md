# **Bomberman**

### "Simples" implemmentação do jogo Bomberman em Haskell para a disciplina de Programação Funcional (2020/2).

Atualmente, os seguintes tipos de dados e as seguintes estruturas de dados norteam o projeto:

## Estruturas:

- `data Item`: os tipos de dados válidos em uma célula (`JogadorX | JogadorY | JogadorW | JogadorZ | Grama | Patins | Arremesso | Fogo | Bomba | Parede | Pedra`); nesse implementação, só é possível quatro jogadores distintos (para mais, só adicionar mais itens de jogadores).

- `data Orientacao`: orientação de jogadores e de movimentação de jogadores (norte `N`, sul `S`, leste `L` e oeste `O`).

- `data Identificador`: identificadores são *tags* para os jogadores (para facilitar a chamada de funções de alterações de estado do jogo).

- `type Jogador`: é o *status* do jogador em determinado estado do jogo: é uma 4-upla contendo um `Identificador`, uma dupla `(Int, Int)` com sua localização no tabuleiro, sua orientação `Orientacao` atual e sua "mochila", consistindo de uma tripla de duplas com a "quantidade" de presentes dos três tipos: `(Patins, Int)`, `(Fogo, Int)` e `(Arremesso, Int)`.

- `type Celula`: uma lista de itens, `[Item]`.
- `type Linha`: uma lista de células, `[Celula]`.
- `type Tabuleiro`: uma lista de linhas, `[Linha]`.

Atualmente, as seguintes funções (de funcionalidade prática para a execução do jogo) foram implementadas:

## Funções:

- `celulaValida`: verifica a validade de uma célula do tabuleiro, ou seja, se não há sobreposições inválidas (por exemplo, `Parede` em cima de `Parede`), a base da pilha (da célula) válida (por `sobreposicoes`) e se a célula não possui elementos repetidos (por `unicidade`).

- `criaTabuleiro`: dado um `Tabuleiro`, ou seja, uma `[Linha]` (ou ainda, uma matriz de células, `[[Celula]]`) cria um tabuleiro e configura os jogadores encontrados nesse tabuleiro e coloca-os em uma lista de jogadores `[Jogador]`, retornando, assim, uma dupla `(Tabuleiro, [Jogador])`, a qual será a estrutura utilizada por todas as funções principais para mudar o estado do jogo. Caso o tabuleiro não seja válido, será levantado um erro. Todos os jogadores encontrados são colocados em `[Jogador]` posicionados para `N` e com a mochila `((Patins,1),(Fogo,1),(Arremesso,1))` (modificável em `setaJogadores`).

- `movimento`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante de jogo), um `Identificador` e uma `Orientacao`, se for possível movimentar o jogador (ele está no tabuleiro e a célula para onde está sua `Orientacao` for válida em movimento, ou seja, for um buraco ou uma grama) referente a esse identificador, o estado do jogo será atualizado.

- `arremesso`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante de jogo), um `Identificador` e uma `Orientacao`, se for possível o jogador `Identificador` (por exemplo, `X`) arremesssar uma bomba (ele está olhando para a bomba, ou seja, a célula vizinha a qual ele está olhando é bomba, ele tem capacidade de `Arremesso` e não há obstáculos no caminho), o estado do jogo será atualizado (bomba some da célula vizinha e aparece na célula a qual foi arremessada). Nesse sentido, a bomba só pode cair em uma `Grama` ou em um buraco `[]`, sendo que nesse caso a bomba cai no "limbo".

- `explosao`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante de jogo) e três valores (`Int`, `Int` e `Int`, referentes à linha, à coluna e à intensidade a qual a bomba explodirá), o estado do jogo será atualizado. Nesse sentido, explodirá com intensidade em linha e em coluna até achar um `Buraco`, `Pedra` ou `Bomba`, não os destruindo, ou até achar um `Jogador` ou uma `Parede`, destruindo-os (caso ache um jogador, a lista de jogadores `[Jogador]` é atualizada). Nessa implementação, `Presente` não para bomba (mas caso uma explosão de bomba encontre-os, eles são destruídos da suas respectivas células), mas que pode muito facilmente será trocada para parar bombar. `Grama` também não para bombas, mas também não é de alguma forma destruída.

- `fim`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante de jogo) e um `Identificador` de jogador, verifica se o jogo chegou ao final, ou seja, se o jogador entrado não está mais no tabuleiro (se de alguma forma ele foi morto, ou seja, caiu em algum buraco ou foi explodido por uma bomba.

- `fim'`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante de jogo), verifica se não há mais jogadores no tabuleiro (para uma implementação de jogo estilo *deathmatch*).

## Exemplos:

Seja `t` sempre um possível tabuleiro `Tabuleiro`. Assim, aguns exemplos de tabuleiros inválidos são:

`t = [[[Patins, Grama], [Grama], [Patins, Grama]], [[JogadorZ, JogadorW, Grama], [Bomba, Grama], [Patins, Grama]], [[Patins, Grama], [Parede, Grama], [Patins, Grama]]]`
`criaTabuleiro t`

`>>> *** Exception: Tabuleiro invalido!`, pois há dois jogadores em uma célula.

`t = [[[Patins, Grama], [Grama], [Patins, Grama]], [[JogadorX, Grama], [Bomba, Bomba, Grama], [Patins, Grama]], [[Patins, Grama], [Parede, Grama], [Patins, Grama]]]`
`criaTabuleiro t`

`>>> *** Exception: Tabuleiro invalido!`, pois há duas bombas em uma célula.

`t = [[[Patins, Grama], [Grama], [Patins, Parede]], [[JogadorX, Grama], [Bomba, Grama], [Patins, Grama]], [[Patins, Grama], [Parede, Grama], [Patins, Grama]]]`
`criaTabuleiro t`

`>>> *** Exception: Tabuleiro invalido!`, pois uma bomba não pode ficar em cima de uma parede.

`t = [[[Patins, Grama], [Grama], [Patins, Parede]], [[JogadorX, Grama], [Grama, Patins], [Patins, Grama]], [[Patins, Grama], [Parede, Grama], [Patins, Grama]]]`
`criaTabuleiro t`

`>>> *** Exception: Tabuleiro invalido!`, pois existe uma grama que não está na base da pilha da célula.

De forma análoga, sejam os seguintes exemplos tabuleiros válidos:

`t = [[[Patins, Grama], [Grama], [Parede]], [[JogadorX, Grama], [Patins, Grama], [Grama]], [[Patins, Grama], [Parede, Grama], [Patins, Grama]]]`
`t0 = criaTabuleiro t`
`t0`

`>>> ([[[Patins,Grama],[Grama],[Parede]],[[JogadorX,Grama],[Patins,Grama],[Grama]],[[Patins,Grama],[Parede,Grama],[Patins,Grama]]],[(X,(2,1),N,((Patins,1),(Fogo,1),(Arremesso,1)))])`, ou seja, uma dupla de estato de jogo `t0` com um tabuleiro válido e uma lista com um só elemento (o único jogador encontrado no tabuleiro). No caso em que não encontre jogadores, a aplicação observará que não há jogadores e finalizará o jogo por `fim'`.

`t = [[[JogadorX, Grama], [Bomba, Grama], [Parede]], [[Grama], [JogadorY, Grama], [Grama]], [[Patins, Grama], [Parede, Grama], [Patins, Grama]]]`
`t0 = criaTabuleiro t`
`t0`

`>>> ([[[JogadorX,Grama],[Bomba,Grama],[Parede]],[[Grama],[JogadorY,Grama],[Grama]],[[Patins,Grama],[Parede,Grama],[Patins,Grama]]],[(X,(1,1),N,((Patins,1),(Fogo,1),(Arremesso,1))),(Y,(2,2),N,((Patins,1),(Fogo,1),(Arremesso,1)))])`, um tabuleiro válido e uma lista com os dois jogadores encontrados no tabuleiro válido.

Agora, modificar o tabuleiro (jogar em si, andando com os jogadores e arremessando bombas) usemos o exemplo acima `t0` (iremos mudar a direção do jogador X para o leste e tentar jogar a bomba que está na célula vizinha:

`t1 = movimento t0 X L`
`t1`

`>>> ([[[JogadorX,Grama],[Bomba,Grama],[Parede]],[[Grama],[JogadorY,Grama],[Grama]],[[Patins,Grama],[Parede,Grama],[Patins,Grama]]],[(X,(1,1),L,((Patins,1),(Fogo,1),(Arremesso,1))),(Y,(2,2),N,((Patins,1),(Fogo,1),(Arremesso,1)))])`

`t2 = arremesso t1 X L`
`t2`

`>>> ([[[JogadorX,Grama],[Bomba,Grama],[Parede]],[[Grama],[JogadorY,Grama],[Grama]],[[Patins,Grama],[Parede,Grama],[Patins,Grama]]],[(X,(1,1),L,((Patins,1),(Fogo,1),(Arremesso,1))),(Y,(2,2),N,((Patins,1),(Fogo,1),(Arremesso,1)))])`, não mudou a posição da bomba por causa da `Parede`.

`t3 = explosao t2 1 2 1`
`t3`

`>>> ([[[Grama],[Grama],[]],[[Grama],[Grama],[Grama]],[[Patins,Grama],[Parede,Grama],[Patins,Grama]]],[])`, saiu explodindo tudo, matando os dois jogadores (por isso `[Jogador]` está vazia) e destruindo uma parede.



