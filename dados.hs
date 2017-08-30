-- Módulo que trata da manipulação de arquivo

module Dados (escrever_arquivo, existe_nome) where

import Control.Exception
import System.IO
import System.IO.Error
import System.Random
import System.Directory


-- função que escreve no arquivo
escrever_arquivo :: String -> IO ()
escrever_arquivo nome = do
            -- abre o arquivo para escrita (modo append adiciona no final)
            -- o arquivo é criado se não existir
            arq <- openFile "nomes.txt" AppendMode
            -- escreve o nome no arquivo
            hPutStrLn arq nome
            -- fecha o arquivo
            hClose arq
            return ()


-- função que verifica se um nome existe no arquivo
existe_nome :: String -> IO Bool
existe_nome nome = do
            {catch (ler_arquivo) tratar_erro;}
            where
                -- tenta ler o arquivo
                ler_arquivo = do
                {
                    -- abre o arquivo para leitura
                    arq <- openFile "nomes.txt" ReadMode;
                    -- ler todo o conteúdo do arquivo
                    nomes <- hGetContents arq;
                    -- se existir retorna True, caso contrário retorna False
                    -- função lines pega uma string e quebra em uma lista
                    -- de strings, utiliza o caractere de nova linha para fazer a quebra
                    if (aux_existe_nome nome (lines nomes)) then do
                        hClose arq;
                        return True
                    else do
                        hClose arq;
                        return False
                }
                tratar_erro erro = if isDoesNotExistError erro then do
                {
                    -- se não existir o arquivo, então abre para escrita
                    arq <- openFile "nomes.txt" WriteMode;
                    -- fecha o arquivo
                    hClose arq;
                    return False
                }
                else
                    ioError erro


-- função auxiliar que verifica se uma string existe em uma lista de strings
aux_existe_nome :: String -> [String] -> Bool
aux_existe_nome _ [] = False
aux_existe_nome nome (x:xs)
                | (nome == x) = True
                | otherwise = aux_existe_nome nome xs

