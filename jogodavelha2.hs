import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import Data.List
import Data.Function

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Control.Concurrent
import Graphics.UI.Gtk.Layout.Grid

-- definindo tipos de dados
type Jogadores = [Jogador]
type Nome = String
type Vez = Int
type Pontuacao = Int
type Tabela = [String]

data Jogador = Jogador Nome Pontuacao deriving (Show, Read)

-- criação de botoes e ação onClick
mkBtn :: String -> MVar Bool -> MVar Tabela -> IO Button
mkBtn label vez tab = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  Graphics.UI.Gtk.on btn buttonActivated $ do 
	  turno <- takeMVar vez
	  label1 <- buttonGetLabel btn
	  if (turno == True) then do -- jogador O
			 if (label1 == " ") then do -- botao nao marcado
			 		set btn [ buttonRelief := ReliefNone, buttonLabel := "O" ]
		 			putMVar vez (not turno)
	  				tabela <- takeMVar tab
	   				result <- verificarVencedor tabela 3 "O"
            -- lançar algo com o fim do jogo/partida
					if (result == True) then do
						 postGUIAsync mainQuit
	   					 liftIO mainQuit
	   				   else do
			   		     putMVar tab tabela
	  			else do
			  		putMVar vez turno
	 		
		else do --jogador X
	  		 if (label1 == " ") then do   -- botao nao marcadon
			 		set btn [ buttonRelief := ReliefNone, buttonLabel := "X" ]
	  				putMVar vez (not turno)
	  				tabela <- takeMVar tab
	   				result <- verificarVencedor tabela 3 "X"
			-- lançar algo com o fim do jogo/partida
					putMVar tab tabela
				else do
					putMVar vez turno	 
  return btn

-- começo novo main
main :: IO ()
main = do
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "Jogo da Velha"
             , windowResizable    := True
             , windowDefaultWidth  := 400
             , windowDefaultHeight := 400 ]
  
  entry <- entryNew
  label <- labelNewWithMnemonic "0 x 0"
  labelSetMnemonicWidget label entry
  
  grid <- gridNew                
  gridSetRowHomogeneous grid True
  let attach x y w h item = gridAttach grid item x y w h 
  
  vez <- newMVar True
  let dificuldade = 3

  if (dificuldade == 3) then do
      tabuleiro3x3 <- newMVar ["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"]
      attach 0 0 3 1 label         
      mkBtn " " vez tabuleiro3x3 >>= attach 0 1 1 1 
      mkBtn " " vez tabuleiro3x3 >>= attach 0 2 1 1
      mkBtn " " vez tabuleiro3x3 >>= attach 0 3 1 1
      mkBtn " " vez tabuleiro3x3 >>= attach 1 1 1 1
      mkBtn " " vez tabuleiro3x3 >>= attach 1 2 1 1
      mkBtn " " vez tabuleiro3x3 >>= attach 1 3 1 1
      mkBtn " " vez tabuleiro3x3 >>= attach 2 1 1 1
      mkBtn " " vez tabuleiro3x3 >>= attach 2 2 1 1
      mkBtn " " vez tabuleiro3x3 >>= attach 2 3 1 1
    else if (dificuldade == 5) then do
      tabuleiro5x5 <- newMVar ["A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5", "C1", "C2", "C3", "C4", "C5", "D1", "D2", "D3", "D4", "D5", "E1", "E2", "E3", "E4", "E5"]
      attach 0 0 5 1 label           
      mkBtn " " vez tabuleiro5x5 >>= attach 0 1 1 1 
      mkBtn " " vez tabuleiro5x5 >>= attach 0 2 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 0 3 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 0 4 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 0 5 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 1 1 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 1 2 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 1 3 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 1 4 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 1 5 1 1 
      mkBtn " " vez tabuleiro5x5 >>= attach 2 1 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 2 2 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 2 3 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 2 4 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 2 5 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 3 1 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 3 2 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 3 3 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 3 4 1 1 
      mkBtn " " vez tabuleiro5x5 >>= attach 3 5 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 4 1 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 4 2 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 4 3 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 4 4 1 1
      mkBtn " " vez tabuleiro5x5 >>= attach 4 5 1 1
    else do
      tabuleiro7x7 <- newMVar ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "G1", "G2", "G3", "G4", "G5", "G6", "G7"]
      attach 0 0 7 1 label           
      mkBtn " " vez tabuleiro7x7 >>= attach 0 1 1 1 
      mkBtn " " vez tabuleiro7x7 >>= attach 0 2 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 0 3 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 0 4 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 0 5 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 0 6 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 0 7 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 1 1 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 1 2 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 1 3 1 1 
      mkBtn " " vez tabuleiro7x7 >>= attach 1 4 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 1 5 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 1 6 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 1 7 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 2 1 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 2 2 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 2 3 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 2 4 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 2 5 1 1 
      mkBtn " " vez tabuleiro7x7 >>= attach 2 6 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 2 7 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 3 1 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 3 2 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 3 3 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 3 4 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 3 5 1 1 
      mkBtn " " vez tabuleiro7x7 >>= attach 3 6 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 3 7 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 4 1 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 4 2 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 4 3 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 4 4 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 4 5 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 4 6 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 4 7 1 1 
      mkBtn " " vez tabuleiro7x7 >>= attach 5 1 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 5 2 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 5 3 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 5 4 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 5 5 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 5 6 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 5 7 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 6 1 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 6 2 1 1 
      mkBtn " " vez tabuleiro7x7 >>= attach 6 3 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 6 4 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 6 5 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 6 6 1 1
      mkBtn " " vez tabuleiro7x7 >>= attach 6 7 1 1


  containerAdd window grid
 
  Graphics.UI.Gtk.on window deleteEvent $ do -- handler to run on window destruction
    liftIO mainQuit
    return False
  
  widgetShowAll window
  mainGUI
-- fim novo main

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
                    result <- verificarVencedor tabela aridade " X"
                    -- verifica se o jogador1 venceu
                    if (result) then do
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
                        result2 <- verificarVencedor tabela aridade " O"
                        if (result2) then do
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


verificarVencedor:: Tabela -> Int -> String -> IO Bool
verificarVencedor tabela 3 simbolo
    -- verifica primeiro nas linhas, atenção: o índice começa do 0
    | (((tabela !! 0) == simbolo) && ((tabela !! 1) == simbolo) && ((tabela !! 2) == simbolo)) = return True
    | (((tabela !! 3) == simbolo) && ((tabela !! 4) == simbolo) && ((tabela !! 5) == simbolo)) = return True
    | (((tabela !! 6) == simbolo) && ((tabela !! 7) == simbolo) && ((tabela !! 8) == simbolo)) = return True
    -- verifica nas colunas
    | (((tabela !! 0) == simbolo) && ((tabela !! 3) == simbolo) && ((tabela !! 6) == simbolo)) = return True
    | (((tabela !! 1) == simbolo) && ((tabela !! 4) == simbolo) && ((tabela !! 7) == simbolo)) = return True
    | (((tabela !! 2) == simbolo) && ((tabela !! 5) == simbolo) && ((tabela !! 8) == simbolo)) = return True
    -- verifica nas diagonais
    | (((tabela !! 0) == simbolo) && ((tabela !! 4) == simbolo) && ((tabela !! 8) == simbolo)) = return True
    | (((tabela !! 2) == simbolo) && ((tabela !! 4) == simbolo) && ((tabela !! 6) == simbolo)) = return True
    | otherwise = return False
verificarVencedor tabela 5 simbolo
    -- verifica primeiro nas linhas, atenção: o índice começa do 0
    | (((tabela !! 0) == simbolo) && ((tabela !! 1) == simbolo) && ((tabela !! 2) == simbolo) && ((tabela !! 3) == simbolo) && ((tabela !! 4) == simbolo)) = return True
    | (((tabela !! 5) == simbolo) && ((tabela !! 6) == simbolo) && ((tabela !! 7) == simbolo) && ((tabela !! 8) == simbolo) && ((tabela !! 9) == simbolo)) = return True
    | (((tabela !! 10) == simbolo) && ((tabela !! 11) == simbolo) && ((tabela !! 12) == simbolo) && ((tabela !! 13) == simbolo) && ((tabela !! 14) == simbolo)) = return True
    | (((tabela !! 15) == simbolo) && ((tabela !! 16) == simbolo) && ((tabela !! 17) == simbolo) && ((tabela !! 18) == simbolo) && ((tabela !! 19) == simbolo)) = return True
    | (((tabela !! 20) == simbolo) && ((tabela !! 21) == simbolo) && ((tabela !! 22) == simbolo) && ((tabela !! 23) == simbolo) && ((tabela !! 24) == simbolo)) = return True
    -- verifica nas colunas
    | (((tabela !! 0) == simbolo) && ((tabela !! 5) == simbolo) && ((tabela !! 10) == simbolo) && ((tabela !! 15) == simbolo) && ((tabela !! 20) == simbolo)) = return True
    | (((tabela !! 1) == simbolo) && ((tabela !! 6) == simbolo) && ((tabela !! 11) == simbolo) && ((tabela !! 16) == simbolo) && ((tabela !! 21) == simbolo)) = return True
    | (((tabela !! 2) == simbolo) && ((tabela !! 7) == simbolo) && ((tabela !! 12) == simbolo) && ((tabela !! 17) == simbolo) && ((tabela !! 22) == simbolo)) = return True
    | (((tabela !! 3) == simbolo) && ((tabela !! 8) == simbolo) && ((tabela !! 13) == simbolo) && ((tabela !! 18) == simbolo) && ((tabela !! 23) == simbolo)) = return True
    | (((tabela !! 4) == simbolo) && ((tabela !! 9) == simbolo) && ((tabela !! 14) == simbolo) && ((tabela !! 19) == simbolo) && ((tabela !! 24) == simbolo)) = return True
    -- verifica nas diagonais
    | (((tabela !! 0) == simbolo) && ((tabela !! 6) == simbolo) && ((tabela !! 12) == simbolo) && ((tabela !! 18) == simbolo) && ((tabela !! 24) == simbolo)) = return True
    | (((tabela !! 4) == simbolo) && ((tabela !! 8) == simbolo) && ((tabela !! 12) == simbolo) && ((tabela !! 16) == simbolo) && ((tabela !! 20) == simbolo)) = return True
    
    | otherwise = return False
verificarVencedor tabela 7 simbolo
    -- verifica primeiro nas linhas, atenção: o índice começa do 0
    | (((tabela !! 0) == simbolo) && ((tabela !! 1) == simbolo) && ((tabela !! 2) == simbolo) && ((tabela !! 3) == simbolo) && ((tabela !! 4) == simbolo) && ((tabela !! 5) == simbolo) && ((tabela !! 6) == simbolo)) = return True 
    | (((tabela !! 7) == simbolo) && ((tabela !! 8) == simbolo) && ((tabela !! 9) == simbolo) && ((tabela !! 10) == simbolo) && ((tabela !! 11) == simbolo) && ((tabela !! 12) == simbolo) && ((tabela !! 13) == simbolo)) = return True
    | (((tabela !! 14) == simbolo) && ((tabela !! 15) == simbolo) && ((tabela !! 16) == simbolo) && ((tabela !! 17) == simbolo) && ((tabela !! 18) == simbolo) && ((tabela !! 19) == simbolo) && ((tabela !! 20) == simbolo)) = return True
    | (((tabela !! 21) == simbolo) && ((tabela !! 22) == simbolo) && ((tabela !! 23) == simbolo) && ((tabela !! 24) == simbolo) && ((tabela !! 25) == simbolo) && ((tabela !! 26) == simbolo) && ((tabela !! 27) == simbolo)) = return True
    | (((tabela !! 28) == simbolo) && ((tabela !! 29) == simbolo) && ((tabela !! 30) == simbolo) && ((tabela !! 31) == simbolo) && ((tabela !! 32) == simbolo) && ((tabela !! 33) == simbolo) && ((tabela !! 34) == simbolo)) = return True
    | (((tabela !! 35) == simbolo) && ((tabela !! 36) == simbolo) && ((tabela !! 37) == simbolo) && ((tabela !! 38) == simbolo) && ((tabela !! 39) == simbolo) && ((tabela !! 40) == simbolo) && ((tabela !! 41) == simbolo)) = return True
    | (((tabela !! 42) == simbolo) && ((tabela !! 43) == simbolo) && ((tabela !! 44) == simbolo) && ((tabela !! 45) == simbolo) && ((tabela !! 46) == simbolo) && ((tabela !! 47) == simbolo) && ((tabela !! 48) == simbolo)) = return True
    -- verifica nas colunas
    | (((tabela !! 0) == simbolo) && ((tabela !! 7) == simbolo) && ((tabela !! 14) == simbolo) && ((tabela !! 21) == simbolo) && ((tabela !! 28) == simbolo) && ((tabela !! 35) == simbolo) && ((tabela !! 42) == simbolo)) = return True 
    | (((tabela !! 1) == simbolo) && ((tabela !! 8) == simbolo) && ((tabela !! 15) == simbolo) && ((tabela !! 22) == simbolo) && ((tabela !! 29) == simbolo) && ((tabela !! 36) == simbolo) && ((tabela !! 43) == simbolo)) = return True
    | (((tabela !! 2) == simbolo) && ((tabela !! 9) == simbolo) && ((tabela !! 16) == simbolo) && ((tabela !! 23) == simbolo) && ((tabela !! 30) == simbolo) && ((tabela !! 37) == simbolo) && ((tabela !! 44) == simbolo)) = return True
    | (((tabela !! 3) == simbolo) && ((tabela !! 10) == simbolo) && ((tabela !! 17) == simbolo) && ((tabela !! 24) == simbolo) && ((tabela !! 31) == simbolo) && ((tabela !! 38) == simbolo) && ((tabela !! 45) == simbolo)) = return True
    | (((tabela !! 4) == simbolo) && ((tabela !! 11) == simbolo) && ((tabela !! 18) == simbolo) && ((tabela !! 25) == simbolo) && ((tabela !! 32) == simbolo) && ((tabela !! 39) == simbolo) && ((tabela !! 46) == simbolo)) = return True
    | (((tabela !! 5) == simbolo) && ((tabela !! 12) == simbolo) && ((tabela !! 19) == simbolo) && ((tabela !! 26) == simbolo) && ((tabela !! 33) == simbolo) && ((tabela !! 40) == simbolo) && ((tabela !! 47) == simbolo)) = return True
    | (((tabela !! 6) == simbolo) && ((tabela !! 13) == simbolo) && ((tabela !! 20) == simbolo) && ((tabela !! 27) == simbolo) && ((tabela !! 34) == simbolo) && ((tabela !! 41) == simbolo) && ((tabela !! 48) == simbolo)) = return True

    -- verifica nas diagonais
    | (((tabela !! 0) == simbolo) && ((tabela !! 8) == simbolo) && ((tabela !! 16) == simbolo) && ((tabela !! 24) == simbolo) && ((tabela !! 32) == simbolo) && ((tabela !! 40) == simbolo) && ((tabela !! 48) == simbolo)) = return True 
    | (((tabela !! 6) == simbolo) && ((tabela !! 12) == simbolo) && ((tabela !! 18) == simbolo) && ((tabela !! 24) == simbolo) && ((tabela !! 30) == simbolo) && ((tabela !! 36) == simbolo) && ((tabela !! 42) == simbolo)) = return True
    
    | otherwise = return False


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
ordenar dados = sortBy (Data.Function.on compare obterPontuacao) dados