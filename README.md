# **Bomberman**

### "Simples" implementação do jogo Bomberman em Haskell para a disciplina de Programação Funcional (2020/2) (Pedro Henrique Marra Araújo - 12011BCC008).

Nessa implementação, o jogador pode andar livremente pelo tabuleiro (contanto que o movimento seja válido), arremessa bombas vizinhas e coloca novas bombas no tabuleiro (contanto que ele tenha capacidade de colocá-las). Infelizmente, não foi implementado as atualizações automáticas das explosões das bombas (possíveis soluções para isso é a utilização de uma *timer* ou um sorteio aleatório de um momento que a bomba explodiria, porém ambas necessitariam de outras bibliotecas e informações que acredito que não cabem ao caráter introdutório da disciplina) e, consequentemente, não foi implementado o tipo de término de explosão de jogador (atualmente os jogadores só morrem caindo em um buraco). Outra possível modificação futura é a entrada de tabuleiros diferentes para o jogo poder ser jogado (atualmente o tabuleiro é fixo como `tab` em `Main.hs`).

Atualmente, os seguintes tipos de dados e as seguintes estruturas de dados norteam o projeto:

## Estruturas:

- `data Item`: os tipos de dados válidos em uma célula (`JogadorX | JogadorY | JogadorW | JogadorZ | Grama | Patins | Arremesso | Fogo | Bomba | Parede | Pedra`); nesse implementação, só é possível quatro jogadores distintos (para mais, só adicionar mais itens de jogadores).

- `data Orientacao`: orientação de jogadores e de movimentação de jogadores (norte `N`, sul `S`, leste `L` e oeste `O`).

- `data Identificador`: identificadores são *tags* para os jogadores (para facilitar a chamada de funções de alterações de estado do jogo).

- `type Jogador`: é o *status* do jogador em determinado estado do jogo: é uma 4-upla contendo um `Identificador`, uma dupla `(Int, Int)` com sua localização no tabuleiro, sua orientação `Orientacao` atual e sua "mochila", consistindo de uma tripla de duplas com a "quantidade" de presentes dos três tipos: `(Patins, Int)`, `(Fogo, Int)` e `(Arremesso, Int)`.

- `type Instante`: uma dupla `(Tabuleiro, [Jogador])`, é um instante válido de jogo, ou seja, consiste de uma tabuleiro válido e uma lista de jogadores referente aos jogadores presentes no tabuleiro (`criaTabuleiro` retorna o instante inicial e todas as outras funções de atualização de jogo recebem um instante, juntamente com informações adicionais, e retornam um outro instante atualizado).

- `type Celula`: uma lista de itens, `[Item]`.

- `type Linha`: uma lista de células, `[Celula]`.

- `type Tabuleiro`: uma lista de linhas, `[Linha]`.

- `data Acao`: ações às quais os jogadores podem escolher (`ColocarBomba` para colocar bomba no tabuleiro, `Arremessar` para arremessar uma bomba vizinha, `Mover` para mover o jogador pelo tabuleiro e `Sair` para sair do jogo).

- `data Codigo`: tipo de dado que oferece especificações para o tipo de término de jogo (`Acabou` quando o jogo é terminado pelo estilo de jogo, no caso por `fim'`, ou seja, *deathmatch*, onde acaba o jogo quando só sobra um no tabuleiro). 

Atualmente, as seguintes funções (de funcionalidade prática para a execução do jogo) foram implementadas:

## Funções Principais:

- `criaTabuleiro`: dado um `Tabuleiro`, ou seja, uma `[Linha]` (ou ainda, uma matriz de células, `[[Celula]]`) cria um tabuleiro e configura os jogadores encontrados nesse tabuleiro e coloca-os em uma lista de jogadores `[Jogador]`, retornando, assim, uma dupla `(Tabuleiro, [Jogador])`, a qual será a estrutura utilizada por todas as funções principais para mudar o estado do jogo. Caso o tabuleiro não seja válido (dimensões incompatíveis com o jogo ou há pelo menos uma célula inválida por `celulaValida`, ou seja, se não há sobreposições inválidas (por exemplo, `Parede` em cima de `Parede`), a base da pilha (da célula) válida (por `sobreposicoes`) e se a célula não possui elementos repetidos (por `unicidade`)), será levantado um erro. Todos os jogadores encontrados são colocados em `[Jogador]` posicionados para `N` e com a mochila `((Patins,1),(Fogo,1),(Arremesso,1))` (modificável em `setaJogadores`).

- `movimento`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante `Instante` de jogo), um `Identificador` e uma `Orientacao`, se for possível movimentar o jogador (ele está no tabuleiro e a célula para onde está sua `Orientacao` for válida em movimento, ou seja, for um buraco ou uma grama) referente a esse identificador, o estado do jogo será atualizado.

- `coloca`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante `Instante` de jogo), um `Identificador` e uma `Orientacao`, se for possível o jogador identificado colocar uma bomba na célula vizinha (de orientação entrada), assim o fará, atualizando a orientação do jogador (terminará olhando para a bomba colocada). Caso contrário, será somente atualizado a orientação do jogador. Será feita essa implementação, pois, por especificação inicial, um jogador só pode estar em cima de uma grama (ou seja, uma bomba e um jogador não podem coexistir em uma mesma célula e, portanto, em um instante de jogo, o jogador não pode colocar uma bomba na sua célula atual).

- `arremesso`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante `Instante` de jogo), um `Identificador` e uma `Orientacao`, se for possível o jogador `Identificador` (por exemplo, `X`) arremesssar uma bomba (ele está olhando para a bomba, ou seja, a célula vizinha a qual ele está olhando é bomba, ele tem capacidade de `Arremesso` e não há obstáculos no caminho), o estado do jogo será atualizado (bomba some da célula vizinha e aparece na célula a qual foi arremessada). Nesse sentido, a bomba só pode cair em uma `Grama` ou em um buraco `[]`, sendo que nesse caso a bomba cai no "limbo".

- `explosao`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante `Instante` de jogo) e três valores (`Int`, `Int` e `Int`, referentes à linha, à coluna e à intensidade a qual a bomba explodirá), o estado do jogo será atualizado. Nesse sentido, explodirá com intensidade em linha e em coluna até achar um `Buraco`, `Pedra` ou `Bomba`, não os destruindo, ou até achar um `Jogador` ou uma `Parede`, destruindo-os (caso ache um jogador, a lista de jogadores `[Jogador]` é atualizada). Nessa implementação, `Presente` não para bomba (mas caso uma explosão de bomba encontre-os, eles são destruídos da suas respectivas células), mas que pode muito facilmente será trocada para parar bombar. `Grama` também não para bombas, mas também não é de alguma forma destruída.

- `fim`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante `Instante` de jogo) e um `Identificador` de jogador, verifica se o jogo chegou ao final, ou seja, se o jogador entrado não está mais no tabuleiro (se de alguma forma ele foi morto, ou seja, caiu em algum buraco ou foi explodido por uma bomba.

- `fim'`: dado uma dupla `(Tabuleiro, [Jogador])` (um instante `Instante` de jogo), verifica se sobrou só um jogador no tabuleiro (para uma implementação de jogo estilo *deathmatch* - é o modo utilizado para a implementação atual, pois é a mais coerente nesse caso).

- `actionLoop`: função que fica atualizando os instantes de jogo dado as informações do tabuleiro e dado as escolhas dos jogadores; é finalizada quando o jogador escolhe acabar o jogo ou sobra só um jogador no tabuleiro.

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

Agora, modificar o tabuleiro (jogar em si, andando com os jogadores e arremessando bombas) usemos o exemplo acima `t0` (iremos mudar a direção do jogador X para o leste e tentar jogar a bomba que está na célula vizinha):

`t1 = movimento t0 X L`
`t1`

`>>> ([[[JogadorX,Grama],[Bomba,Grama],[Parede]],[[Grama],[JogadorY,Grama],[Grama]],[[Patins,Grama],[Parede,Grama],[Patins,Grama]]],[(X,(1,1),L,((Patins,1),(Fogo,1),(Arremesso,1))),(Y,(2,2),N,((Patins,1),(Fogo,1),(Arremesso,1)))])`

`t2 = arremesso t1 X L`
`t2`

`>>> ([[[JogadorX,Grama],[Bomba,Grama],[Parede]],[[Grama],[JogadorY,Grama],[Grama]],[[Patins,Grama],[Parede,Grama],[Patins,Grama]]],[(X,(1,1),L,((Patins,1),(Fogo,1),(Arremesso,1))),(Y,(2,2),N,((Patins,1),(Fogo,1),(Arremesso,1)))])`, não mudou a posição da bomba por causa da `Parede`.

`t3 = explosao t2 1 2 1`
`t3`

`>>> ([[[Grama],[Grama],[]],[[Grama],[Grama],[Grama]],[[Patins,Grama],[Parede,Grama],[Patins,Grama]]],[])`, saiu explodindo tudo, matando os dois jogadores (por isso `[Jogador]` está vazia) e destruindo uma parede.



## Funções Auxiliares:

São funções que auxiliam os processos das funções que serão acionadas no jogo. Algumas se tratam de busca em um tabuleiro, por exemplo `celula` ou `nthLinha`, outras são de avaliações lógicas, por exemplo `unicidade` ou `temJogador`, e outras são de atualização de tabuleiro, por exemplo `atualizaTab` ou `atualizaCelula`:

- `buraco`: verifica se uma célula é um buraco.
- `unicidade`: verifica se uma célula é única (não há itens repetidos na pilha).
- `sobreposicoes`: verifica se uma célula é válida em relação às regras de empilhamento do jogo.
- `celulaValida`: verifica se uma célula é válida (em relação às regras das funções acima).

- `dimensoesValidas`: verifica se a dimensão do tabuleiro é válida (retangular com o tamanho das linhas e a quantidade de colunas no mínimo três).
- `setaJogadores`: dado um tabuleiro, é uma função auxiliar de `criaTabuleiro` que cria a lista de jogadores (com seus respectivos atributos).
- `jogadorItem`: dado um identificador, por exemplo `X`, retorna seu respectivo item (uma vez que o tabuleiro é um `[[[Item]]]`.
- `jogadorIdentificador`: dado um item de jogador, por exemplo `JogadorX`, retorna seu respectivo identificador.
- `jogador`: verifica se item é um jogador.
- `temJogadorX`: verifica se, em um tabuleiro dado, há o determinado jogador (identificador) entrado.
- `listaJogadorX`: dado uma lista de jogadores e um identificador, retorna o jogador referente ao identificador entrado caso houver; caso contrário, levanta um erro.
- `temJogador`: dado um tabuleiro, verifica se há pelo menos um jogador nele.
- `atualizaJogadores`: dado uma lista de jogador e um jogador, caso esse jogador pertença à lista, suas informações serão atualizadas.
- `buscaJogadores`: dado um tabuleiro, retorna uma lista de itens de jogadores.
- `vizinho`: dado um tabuleiro e uma posição de tabuleiro (valores da linha `l` e coluna `c`) e uma orientação `mov`, retorna uma dupla `(Celula, (Int, Int))` referente à célula do vizinho e sua posição no tabuleiro.
- `vizinhoValido`: verifica se a posição de um vizinho é válido (por convenção, `vizinho` retorna `([], (0, 0))` caso inválido).
- `atualizaJogadoresMovimento`: dado uma lista de jogadores e um jogador, atualiza a lista de acordo com os valores do jogador entrado (por exemplo, caso o jogador esteja "fora" do tabuleiro, ou seja, caiu no "limbo", e esteja na lista de jogadores, ele é retirado dela).
- `movimentoInvalido`: verifica se um movimento é inválido (se é um movimento de borda, ou seja, saindo para fora do tabuleiro).
- `atualizaTab`: dado um tabuleiro, uma posição de célula e uma célula a ser trocada, retorna o tabuleiro com a célula atualizada.
- `atualizaCelula`: dado um tabuleiro, uma linha, uma posição de coluna e uma nova célula, atualiza-a na posição entrada e retorna o tabuleiro atualizado.
- `atualizaLinha`: análoga à acima, atualiza uma linha completa em um tabuleiro e retorna-o.
- `atualizaColuna`: análoga à acima, atualiza uma coluna completa em um tabuleiro e retorna-o.
- `nthColuna`: pega a n-ésima coluna de um tabuleiro, retornando-a.
- `nthLinha`: pega a n-ésima linha de um tabuleiro, retornando-a.
- `celula`: pega a célula do tabuleiro referente aos valores de posição entrados, retornando-a.
- `arremessa`: dado um tabuleiro e um jogador, com uma posição e uma orientação, arremessa uma bomba vizinha, caso exista e seja possível arremessá-la, retornando o tabuleiro atualizado.
- `ultimaCelulaValida`: dado um tabuleiro, uma posição de tabuleiro, uma orientação e uma potência de bomba, retorna as informações da última célula válida que uma bomba poderia ser arremessada nessa direção. Nessa implementação, considerou-se que a bomba não "passa por cima" de jogadores, presentes, bombas, pedras e nem paredes (ou seja, uma implementação tradicional de um jogo de Bomberman.
- `atualizaJogadoresExplosao`: análogo ao `atualizaJogadoresMovimento`, dado um tabuleiro e uma lista de jogadores, atualiza-a referente às informações do tabuleiro (por exemplo, se o jogador foi explodido, a função retira jogador da lista).
- `explodeLinha`: função auxiliar do `explosao`, dado um tabuleiro, uma posição de tabuleiro e uma intensidade de bomba, explode todas as células "explodíveis" em relação à linha da bomba.
- `explodeColuna`: análoga à acima, só que em relação à coluna.
- `impressaoJogadores`: imprime os dados de todos os jogadores em uma dada lista de jogadores.
- `impressaoTabuleiro`: dado um tabuleiro, imprime-o formatadamente.
- `pegaJogador`: entra em loop até o usuário entrar com o `Identificador` de jogador válido.
- `pegaAcao`: entra em loop até o usuário entrar com uma ação válida.

