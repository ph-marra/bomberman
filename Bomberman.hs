import Data.Char
import System.Process
import System.Info
import Bomberman

data Acao = ColocarBomba | Arremessar | Mover | Sair
    deriving (Show, Eq)

data Codigo = Acabou | Saiu deriving (Show, Eq)

clearPrompt :: String
clearPrompt
    | os == "darwin" || os == "linux" || os == "linux-android" = "clear"
    | otherwise = "cls"

tab = [[[JogadorX, Grama], [Bomba, Grama], [Parede]], [[], [JogadorY, Grama], [Grama]], [[Patins, Grama], [Parede, Grama], [Patins, Grama]]]

main :: IO ()
main = do
        (cod, id) <- actionLoop i0
        case cod of
            Saiu -> putStr "Fim de jogo, pois escolheu sair!\n\n"
            Acabou -> putStr $ "Fim de jogo. Vencedor = " ++ show id ++ "!\n\n"
        return ()
    where i0 = criaTabuleiro tab

impressaoJogadores :: [Jogador] -> IO ()
impressaoJogadores [] = return ()
impressaoJogadores ((id, _, orient, ((_, p), (_, f), (_, a))):js) = do
    putStr $ "   " ++ show id ++ "    |    " ++ show orient ++ "    |   " ++ show p ++ "    |  " ++ show f ++ "   |     " ++ show a ++ "\n"
    impressaoJogadores js

impressaoTabuleiro :: Tabuleiro -> IO ()
impressaoTabuleiro [] = return ()
impressaoTabuleiro (l:ls) = do
    impressaoTabuleiro' l
    putStr "\n"
    impressaoTabuleiro ls

impressaoTabuleiro' :: Linha -> IO ()
impressaoTabuleiro' [] = return ()
impressaoTabuleiro' (c:cs) = do
    putStr $ formatacaoTabuleiro $ c
    putStr "\t"
    impressaoTabuleiro' cs

formatacaoTabuleiro :: Celula -> String
formatacaoTabuleiro celula
    | null celula = "Buraco      "
    | c == Fogo = (show c) ++ "        "
    | c == Grama || c == Bomba || c == Pedra = (show c) ++ "       "
    | c == Patins || c == Parede = (show c) ++ "      "
    | jogador c = (show c) ++ "    "
    | c == Arremesso = (show c) ++ "   "
    where c = head celula



actionLoop :: Instante -> IO (Codigo, Identificador)
actionLoop ins@(t, js@((fstid, _, _, _):j)) = do
    system $ clearPrompt
    if fim' ins then return (Acabou, fstid) else do
        putStr "Jogador | Direcao | Patins | Fogo | Arremesso\n"
        impressaoJogadores js
        putStr "\n"
        impressaoTabuleiro t
        putStrLn "\nEntre com o identificador do jogador."
        identidade <- pegaJogador
        let (_, _, o, _) = listaJogadorX js identidade
        putStrLn "Entre com o movimento a ser realizado."
        (acao, orient) <- pegaAcao
        case acao of
            ColocarBomba -> actionLoop $ coloca ins identidade orient
            Mover -> actionLoop $ movimento ins identidade orient
            Arremessar -> actionLoop $ arremesso ins identidade o
            Sair -> return (Saiu, X)

pegaJogador :: IO Identificador
pegaJogador =
    do
        j <- getChar
        case toLower j of    
            'x' -> return X
            'y' -> return Y
            'w' -> return W
            'z' -> return Z
            _ -> pegaJogador

pegaAcao :: IO (Acao, Orientacao)
pegaAcao =
    do
        a <- getChar
        case toLower a of
            '2' -> return (ColocarBomba, S)
            '4' -> return (ColocarBomba, O)
            '6' -> return (ColocarBomba, L)
            '8' -> return (ColocarBomba, N)
            'r' -> return (Arremessar, S)
            'w' -> return (Mover, N)
            's' -> return (Mover, S)
            'd' -> return (Mover, L)
            'a' -> return (Mover, O)
            'q' -> return (Sair, S)
            _ -> pegaAcao
