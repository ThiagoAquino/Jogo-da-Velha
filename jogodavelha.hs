import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import Data.List
import Data.Function


-- definindo tipos de dados
type Jogadores = [Jogador]
type Nome = String
type Vez = Int
type Pontuacao = Int
type Tabela = [String]

data Jogador = Jogador Nome Pontuacao deriving (Show, Read)

-- funcao que recebe uma String e retorna um IO String
getString :: String -> IO String
getString str = do
    putStr str
    res <- getLine
    return res

-- funcao para iniciar o jogo
inicio :: IO()
inicio = do
    {catch (ler_arquivo) tratar_erro;}
    where
        ler_arquivo = do
        {
            arq <- openFile "dados.txt" ReadMode; -- abre o arquivo para leitura
            dados <- hGetLine arq; --ler o conteudo do arquivo
            hClose arq; --fecha o arquivo
            menu (read dados); -- passa os dados para a funcao menu
            return ()
        }
        tratar_erro erro = if isDoesNotExistError erro then do 
        {
            arq <- openFile "dados.txt" WriteMode; --abre o arquivo para escrita
            hPutStrLn arq "[]"; -- escreve uma lista vazia no arquivo
            hClose arq; -- fecha o arquivo
            menu []; --passa uma lista vazia para o menu
            return ()
        }
        else
            ioError erro

-- funcao que exibe o Menu inicial
menu :: Jogadores -> IO Jogadores
menu dados = do
    system "cls"
    putStrLn "--------------- Jogo da Velha ----------------"
    putStrLn "\nDigite 1 para cadastrar jogador"
    putStrLn "Digite 2 para jogar"
    putStrLn "Digite 3 para visualizar o ranking"
    putStrLn "Digite 0 para sair"
    putStr $ "Opção: "
    op <- getChar
    getChar -- descarta o enter
    executarOpcao dados op


-- funcao para manipular a opção escolhida pelo usuário
executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dados '1' = cadastrarJogador dados
executarOpcao dados '2' = prepararJogo dados
executarOpcao dados '3' = do
    putStrLn "\nRanking dos jogadores:\n"
    if (null dados) then do
        putStrLn ("Não há jogadores cadastrados!")
    else 
    --a funcao exibe o ranking de forma crescente
        exibirRanking (reverse (ordenar dados))
    putStr "\nPressione Enter para voltar ao menu"
    getChar
    menu dados

executarOpcao dados '0' = do
    putStrLn ("\nAté a próxima!\n")
    return dados

executarOpcao dados _ = do
    putStrLn ("\nOpção inválida! Tente novamente")
    putStrLn ("\nPressione <Enter> para voltar ao menu")
    getChar
    menu dados

--funcao que cadastra jogadores
cadastrarJogador :: Jogadores -> IO Jogadores
cadastrarJogador dados = do
    nome <- getString "\nDigite um nome de usuário: "
    if (existeJogador dados nome) then do
        putStrLn "\nEsse nome já existe, escolha outro"
        putStr "\nPressione enter para continuar"
        getChar
        menu dados
    else do
        arq <- openFile "dados.txt" WriteMode --abre o arquivo para escrita
        hPutStrLn arq (show ((Jogador nome 0):dados))
        hClose arq -- fechar arquivo
        putStrLn ("\nUsuário " ++ nome ++ " cadastrado com sucesso")
        putStrLn ("\nPressione Enter para continuar")
        getChar
        menu ((Jogador nome 0):dados) --retorna a nova lista para o menu

--verifica se o jogador ja existe
existeJogador :: Jogadores -> Nome -> Bool
existeJogador [] _ = False
existeJogador ((Jogador n p):xs) nome
 | (n == nome) = True
 | otherwise = existeJogador xs nome

--funcao que prepara o inicio do jogo
prepararJogo :: Jogadores -> IO Jogadores
prepararJogo dados = do
    jogador1 <- getString "\nDigite o nome do primeiro jogador: "
    if not (existeJogador dados jogador1) then do
        putStrLn "\nEsse jogador não existe!"
        putStr "\nPressione Enter para continuar"
        getChar --descarta o enter
        menu dados
    else do
        jogador2 <- getString "\nDigite o nome do segundo jogador: "
        if not (existeJogador dados jogador2) then do
        putStrLn "\nEsse jogador não existe!"
        putStr "\nPressione Enter para continuar"
        getChar --descarta o enter
        menu dados
        else do
            --aqui os dois jogadores existem
            novoJogo dados jogador1 jogador2

novoJogo :: Jogadores -> Nome -> Nome -> IO Jogadores
novoJogo dados jogador1 jogador2 = do
    putStrLn ("\nIniciando o jogo "  ++ jogador1 ++ " vs " ++ jogador2)
    putStrLn ("\nOs quadrados que possuem números não estão marcados")
    putStrLn ("\n" ++ jogador1 ++ " será o 'X' e " ++ jogador2 ++ " será o 'O'")

    putStrLn "\nEscolha em qual tipo de tabuleiro você quer usar: "
    putStrLn "1 para 3x3"
    putStrLn "2 para 5x5"
    putStrLn "3 para 7x7"

    tipoJogo <- getChar

    let tabuleiro3x3 = ["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"]
    let tabuleiro5x5 = ["A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5", "C1", "C2", "C3", "C4", "C5", "D1", "D2", "D3", "D4", "D5", "E1", "E2", "E3", "E4", "E5"]
    let tabuleiro7x7 = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "G1", "G2", "G3", "G4", "G5", "G6", "G7"]

    case tipoJogo of '1' -> rodarJogo 3 dados tabuleiro3x3 tabuleiro3x3 jogador1 jogador2 0
                     '2' -> rodarJogo 5 dados tabuleiro5x5 tabuleiro5x5 jogador1 jogador2 0
                     '3' -> rodarJogo 7 dados tabuleiro7x7 tabuleiro7x7 jogador1 jogador2 0
                     _ -> do
                        putStrLn "\nFormato inválido"
                        novoJogo dados jogador1 jogador2

imprimirTabuleiro :: Int -> Tabela -> IO()
imprimirTabuleiro 3 tabela = 
    putStrLn ("\n" ++ "                              " ++
        (show (tabela !! 0)) ++ " | " ++ (show (tabela !! 1)) ++ " | " ++ (show (tabela !! 2)) ++
        "\n                              ------------------\n" ++ "                              " ++
        (show (tabela !! 3)) ++ " | " ++ (show (tabela !! 4)) ++ " | " ++ (show (tabela !! 5)) ++
        "\n                              ------------------\n" ++ "                              " ++
        (show (tabela !! 6)) ++ " | " ++ (show (tabela !! 7)) ++ " | " ++ (show (tabela !! 8)) ++
        "\n")
imprimirTabuleiro 5 tabela = 
    putStrLn ("\n" ++ "                              " ++
        (show (tabela !! 0)) ++ " | " ++ (show (tabela !! 1)) ++ " | " ++ (show (tabela !! 2)) ++  " | " ++ (show (tabela !! 3)) ++ " | " ++ (show (tabela !! 4)) ++
        "\n                              --------------------------------\n" ++ "                              " ++
        (show (tabela !! 5)) ++ " | " ++ (show (tabela !! 6)) ++ " | " ++ (show (tabela !! 7)) ++  " | " ++ (show (tabela !! 8)) ++ " | " ++ (show (tabela !! 9)) ++
        "\n                              --------------------------------\n" ++ "                              " ++
        (show (tabela !! 10)) ++ " | " ++ (show (tabela !! 11)) ++ " | " ++ (show (tabela !! 12)) ++  " | " ++ (show (tabela !! 13)) ++ " | " ++ (show (tabela !! 14)) ++
        "\n                              --------------------------------\n" ++ "                              " ++
        (show (tabela !! 15)) ++ " | " ++ (show (tabela !! 16)) ++ " | " ++ (show (tabela !! 17)) ++  " | " ++ (show (tabela !! 18)) ++ " | " ++ (show (tabela !! 19)) ++
        "\n                              --------------------------------\n" ++ "                              " ++
        (show (tabela !! 20)) ++ " | " ++ (show (tabela !! 21)) ++ " | " ++ (show (tabela !! 22)) ++  " | " ++ (show (tabela !! 23)) ++ " | " ++ (show (tabela !! 24)) ++
        "\n")
imprimirTabuleiro 7 tabela = 
    putStrLn ("\n" ++ "                              " ++
        (show (tabela !! 0)) ++ " | " ++ (show (tabela !! 1)) ++ " | " ++ (show (tabela !! 2)) ++  " | " ++ (show (tabela !! 3)) ++ " | " ++ (show (tabela !! 4)) ++ " | " ++ (show (tabela !! 5)) ++ " | " ++ (show (tabela !! 6)) ++
        "\n                              ----------------------------------------------\n" ++ "                              " ++
        (show (tabela !! 7)) ++ " | " ++ (show (tabela !! 8)) ++ " | " ++ (show (tabela !! 9)) ++  " | " ++ (show (tabela !! 10)) ++ " | " ++ (show (tabela !! 11)) ++ " | " ++ (show (tabela !! 12)) ++ " | " ++ (show (tabela !! 13)) ++
        "\n                              ----------------------------------------------\n" ++ "                              " ++
        (show (tabela !! 14)) ++ " | " ++ (show (tabela !! 15)) ++ " | " ++ (show (tabela !! 16)) ++  " | " ++ (show (tabela !! 17)) ++ " | " ++ (show (tabela !! 18)) ++ " | " ++ (show (tabela !! 19)) ++ " | " ++ (show (tabela !! 20)) ++
        "\n                              ----------------------------------------------\n" ++ "                              " ++
        (show (tabela !! 21)) ++ " | " ++ (show (tabela !! 22)) ++ " | " ++ (show (tabela !! 23)) ++  " | " ++ (show (tabela !! 24)) ++ " | " ++ (show (tabela !! 25)) ++ " | " ++ (show (tabela !! 26)) ++ " | " ++ (show (tabela !! 27)) ++
        "\n                              ----------------------------------------------\n" ++ "                              " ++
        (show (tabela !! 28)) ++ " | " ++ (show (tabela !! 29)) ++ " | " ++ (show (tabela !! 30)) ++  " | " ++ (show (tabela !! 31)) ++ " | " ++ (show (tabela !! 32)) ++ " | " ++ (show (tabela !! 33)) ++ " | " ++ (show (tabela !! 34)) ++
        "\n                              ----------------------------------------------\n" ++ "                              " ++
        (show (tabela !! 35)) ++ " | " ++ (show (tabela !! 36)) ++ " | " ++ (show (tabela !! 37)) ++  " | " ++ (show (tabela !! 38)) ++ " | " ++ (show (tabela !! 39)) ++ " | " ++ (show (tabela !! 40)) ++ " | " ++ (show (tabela !! 41)) ++
        "\n                              ----------------------------------------------\n" ++ "                              " ++
        (show (tabela !! 42)) ++ " | " ++ (show (tabela !! 43)) ++ " | " ++ (show (tabela !! 44)) ++  " | " ++ (show (tabela !! 45)) ++ " | " ++ (show (tabela !! 46)) ++ " | " ++ (show (tabela !! 47)) ++ " | " ++ (show (tabela !! 48)) ++
        "\n")


-- memoria serve para ser comparada com o tabuleiro atual e checar se ainda ha condicoes de se jogar
rodarJogo :: Int -> Jogadores -> Tabela -> Tabela -> Nome -> Nome -> Vez -> IO Jogadores
rodarJogo aridade dados tabela memoria jogador1 jogador2 vez = do
                    -- imprime o tabuleiro
                    
                    imprimirTabuleiro aridade tabela

                    -- verifica se o jogador1 venceu
                    if (verificarVencedor tabela aridade " X") then do
                        putStrLn ("Parábens " ++ jogador1 ++ "! Você venceu!!")

                        -- abre o arquivo para escrita para atualizá-lo
                        arq_escrita <- openFile "dados.txt" WriteMode
                        hPutStrLn arq_escrita (show (atualizaPontuacao dados jogador1))
                        hClose arq_escrita

                        -- abre o arquivo para leitura
                        arq_leitura <- openFile "dados.txt" ReadMode
                        dados_atualizados <- hGetLine arq_leitura
                        hClose arq_leitura

                        putStr "\nPressione <Enter> para voltar ao menu..."
                        getChar
                        menu (read dados_atualizados)
                    else do
                        -- verifica se o jogador2 venceu
                        if (verificarVencedor tabela aridade " O") then do
                            putStrLn ("Parábens " ++ jogador2 ++ "! Você venceu!!")

                            -- abre o arquivo para escrita para atualizá-lo
                            arq_escrita <- openFile "dados.txt" WriteMode
                            hPutStrLn arq_escrita (show (atualizaPontuacao dados jogador2))
                            hClose arq_escrita

                            -- abre o arquivo para leitura
                            arq_leitura <- openFile "dados.txt" ReadMode
                            dados_atualizados <- hGetLine arq_leitura
                            hClose arq_leitura

                            putStr "\nPressione <Enter> para voltar ao menu..."
                            getChar
                            menu (read dados_atualizados)
                        else do
                            -- verifica se houve empate
                            -- se o tamanho da intersecção entre "123456789" e "tabela" for 0, então deu empate
                            if ((length (intersect memoria tabela)) == 0) then do
                                putStrLn ("Deu empate!")
                                putStr "\nPressione <Enter> para voltar ao menu..."
                                getChar
                                menu dados
                            else do
                                -- verifica se a vez é do jogador1
                                if (vez == 0) then do
                                    putStr (jogador1 ++ ", é a sua vez! Onde você quer marcar? ")
                                    op <- getLine
                                    --getChar -- descarta o Enter
                                    -- testa se a opção é válida
                                    if not (elem op memoria) then do
                                        putStrLn "\nEssa opção NÃO é válida, tente novamente..."
                                        -- como foi opção inválida, então ainda é a vez do jogador1
                                        rodarJogo aridade dados tabela memoria jogador1 jogador2 0
                                    else
                                        -- se caiu aqui, então é uma opção válida
                                        -- testa se a opção já foi marcada
                                        -- se ela não existir na tabela, é porque já foi marcada
                                        if not (elem op tabela) then do
                                            putStrLn "\nEssa opção já foi marcada, escolha outra opção..."
                                            rodarJogo aridade dados tabela memoria jogador1 jogador2 0
                                        else
                                            -- se caiu aqui é porque a opção é válida e ainda NÃO foi marcada
                                            -- passa 1 para indicar que a vez é do jogador2
                                            -- a nova tabela será o retorno da função obterNovoTabuleiro
                                            rodarJogo aridade dados (obterNovoTabuleiro tabela vez op) memoria jogador1 jogador2 1
                                else do
                                    putStr (jogador2 ++ ", é a sua vez! Onde você quer marcar? ")
                                    op <- getLine
                                    --getChar -- descarta o Enter
                                    if not (elem op memoria) then do
                                        putStrLn "\nEssa opção NÃO é válida, tente novamente..."
                                        -- como foi opção inválida, então ainda é a vez do jogador2
                                        rodarJogo aridade dados tabela memoria jogador1 jogador2 1
                                    else
                                        if not (elem op tabela) then do
                                            putStrLn "\nEssa opção já foi marcada, escolha outra opção..."
                                            rodarJogo aridade dados tabela memoria jogador1 jogador2 1
                                        else
                                            -- se caiu aqui é porque a opção é válida e ainda NÃO foi marcada
                                            -- passa 0 para indicar que a vez é do jogador1
                                            -- a nova tabela será o retorno da função obterNovoTabuleiro
                                            rodarJogo aridade dados (obterNovoTabuleiro tabela vez op) memoria jogador1 jogador2 0

obterNovoTabuleiro :: Tabela -> Vez -> String -> Tabela
obterNovoTabuleiro (x:xs) vez e
 | ((x == e) && (vez == 0)) = ([" X"] ++ xs)
 | ((x == e) && (vez == 1)) = ([" O"] ++ xs)
 | otherwise = x:(obterNovoTabuleiro xs vez e)

verificarVencedor:: Tabela -> Int -> String -> Bool
verificarVencedor tabela aridade simbolo
    -- verifica primeiro nas linhas, atenção: o índice começa do 0
    | (verificaVencedorLinhas tabela aridade simbolo) = True
    -- verifica nas colunas
    | (verificaVencedorColunas tabela aridade simbolo) = True
    -- verifica nas diagonais
    | (verificaVencedorDiagonais tabela aridade simbolo) = True
    | otherwise = False


verificaVencedorDiagonais :: Tabela -> Int -> String -> Bool
verificaVencedorDiagonais tabela aridade simbolo = verificaDiagonal tabela inicios aridade simbolo
    where
        inicios = [0, (aridade-1)]::[Int] -- [0, 2]

verificaDiagonal :: Tabela -> [Int] -> Int -> String -> Bool
verificaDiagonal tabela [diag1,diag2] aridade simbolo
    | (verificarIntervaloDiagonal1 tabela diag1 aridade simbolo) == True = True
    | (verificarIntervaloDiagonal2 tabela diag2 aridade simbolo) == True = True
    | otherwise = False

-- intervalo é de 1 em 1
verificarIntervaloDiagonal1 :: Tabela -> Int -> Int -> String -> Bool
verificarIntervaloDiagonal1 tabela inicio aridade simbolo = checaSimboloEmPosicoesEspecificas tabela posicoes (aridade*aridade) simbolo 0
    where
        posicoes = [inicio, (inicio + aridade + 1)..((aridade * aridade) - 1)]::[Int]

verificarIntervaloDiagonal2 :: Tabela -> Int -> Int -> String -> Bool
verificarIntervaloDiagonal2 tabela inicio aridade simbolo = checaSimboloEmPosicoesEspecificas tabela posicoes (aridade*aridade) simbolo 0
    where
        posicoes = [inicio, (inicio + aridade - 1)..(((aridade-1) * aridade))]::[Int]


verificaVencedorLinhas :: Tabela -> Int -> String -> Bool
verificaVencedorLinhas tabela aridade simbolo = verificaLinha tabela inicios aridade simbolo
    where
        inicios = [0, aridade..((aridade * aridade)-1)]::[Int] -- [0, 3, 6]

verificaLinha :: Tabela -> [Int] -> Int -> String -> Bool
verificaLinha tabela (inicio:listaInicios) aridade simbolo
    | (verificarIntervaloLinha tabela inicio aridade simbolo) == True = True
    | otherwise = verificaLinha tabela listaInicios aridade simbolo
verificaLinha tabela [] _ _ = False

-- intervalo é de 1 em 1
verificarIntervaloLinha :: Tabela -> Int -> Int -> String -> Bool
verificarIntervaloLinha tabela inicio aridade simbolo = checaSimboloEmPosicoesEspecificas tabela posicoes (inicio+aridade) simbolo 0
    where
        posicoes = [inicio, (inicio + 1)..(inicio + aridade - 1)]::[Int]


verificaVencedorColunas :: Tabela -> Int -> String -> Bool
verificaVencedorColunas tabela aridade simbolo = verificaColuna tabela inicios aridade simbolo
    where
        inicios = [0, 1..(aridade-1)]::[Int] -- [0, 1, 2]

verificaColuna :: Tabela -> [Int] -> Int -> String -> Bool
verificaColuna tabela (inicio:listaInicios) aridade simbolo
    | (verificarIntervaloColuna tabela inicio aridade simbolo) == True = True
    | otherwise = verificaColuna tabela listaInicios aridade simbolo
verificaColuna tabela [] _ _ = False

-- intervalo é de aridade em (aridade + aridade)
verificarIntervaloColuna :: Tabela -> Int -> Int -> String -> Bool
verificarIntervaloColuna tabela inicio aridade simbolo = checaSimboloEmPosicoesEspecificas tabela posicoes (aridade * aridade) simbolo 0
    where
        posicoes = [inicio, (inicio + aridade)..((aridade * aridade)-1)]


checaSimboloEmPosicoesEspecificas :: Tabela -> [Int] -> Int -> String -> Int -> Bool
checaSimboloEmPosicoesEspecificas _ [] _ _ acc 
    | acc > 0 = True
    | otherwise = False
checaSimboloEmPosicoesEspecificas tabela (posicao:posicoes) limite simbolo acc
    | posicao >= limite && acc > 0 = True
    | posicao >= limite && acc == 0 = False
    | (tabela !! posicao) == simbolo = checaSimboloEmPosicoesEspecificas tabela posicoes limite simbolo (acc+1)
    | otherwise = False

atualizaPontuacao :: Jogadores -> String -> Jogadores
atualizaPontuacao ((Jogador nome pontuacao):xs) vencedor 
 | (nome == vencedor) = [(Jogador nome (pontuacao + 1))] ++ xs
 | otherwise = (Jogador nome pontuacao):(atualizaPontuacao xs vencedor)

exibirRanking :: Jogadores -> IO()
exibirRanking [] = return()
exibirRanking (x:xs) = do
    putStrLn ((obterNome x) ++ " possui " ++ (show (obterPontuacao x)) ++ " pontos.")
    exibirRanking xs

obterNome :: Jogador -> Nome
obterNome (Jogador nome _) = nome

obterPontuacao :: Jogador -> Pontuacao
obterPontuacao (Jogador _ pontuacao) = pontuacao

ordenar :: Jogadores -> Jogadores
ordenar dados = sortBy (compare `on` obterPontuacao) dados