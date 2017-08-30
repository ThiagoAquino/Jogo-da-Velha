module Main where

import Dados
import Graphics.UI.Gtk


main :: IO ()
main = do

    -- função chamada em todas as aplicações Gtk2Hs
    initGUI

    -- cria uma janela
    window <- windowNew
    -- seta as configurações da janela
    set window [windowTitle := "Cadastrando jogadores",
                windowDefaultWidth := 1000,
                windowDefaultHeight := 1000,
                containerBorderWidth := 10]

    -- a função onDestroy recebe um widget e uma instrução como argumentos
    -- nesse caso recebe window e a função mainQuit
    onDestroy window mainQuit

    -- cria o label do nome
    label_nome <- labelNew (Just "Digite o nome da pessoa:")
    -- seleciona uma fonte para o label_nome
    fonte_label_nome <- fontDescriptionFromString "Arial 15"
    -- seta essa fonte para o label_nome
    widgetModifyFont label_nome (Just fonte_label_nome)
    -- seta a cor para azul
    -- RGB: red green blue
    -- 0xffff é um número de 16 bits com todos os bits em 1
    -- (0 0 0xffff) seleciona a cor azul
    widgetModifyFg label_nome StateNormal (Color 0 0 0xffff)

    -- cria uma entrada de texto para o usuário inserir o nome
    entry_nome <- entryNew

    -- cria um label para informar que o nome foi cadastrado com sucesso
    label_cadastro_info <- labelNew (Just "")
    -- cria uma fonta para o label
    fonte_label_cadastro_info <- fontDescriptionFromString "Arial Bold 10"
    -- seta essa fonte para o label_cadastro_info
    widgetModifyFont label_cadastro_info (Just fonte_label_cadastro_info)

    -- cria o botão de cadastrar nome
    btn_cadastro <- buttonNewWithLabel "Cadastrar nome"
    -- associa o evento do clique à função "cadastrar" passando a entry e o label
    btn_cadastro `onClicked` cadastrar entry_nome label_cadastro_info


    -- cria um layout
    -- VBox é um container que organiza os widgets filhos em uma única coluna
    -- vBoxNew cria um novo VBox
    -- True se todos os filhos terão o mesmo espaço
    -- Int é o espaço, o número de pixels entre os filhos
    lay <- vBoxNew False 0

    -- adiciona os widgets ao layout
    containerAdd lay label_nome
    containerAdd lay entry_nome
    containerAdd lay btn_cadastro
    containerAdd lay label_cadastro_info
    
    -- adiciona o layout à janela
    containerAdd window lay

    -- mostra a janela com todos os widgets
    widgetShowAll window

    -- main loop da aplicação
    mainGUI


-- função associada ao clique do botão "Cadastrar nome"
cadastrar :: Entry -> Label -> IO ()
cadastrar entry_nome label_cadastro_info = do
            -- obtém o texto da entrada de texto
            nome <- entryGetText entry_nome
            -- testa se o nome está vazio
            if (nome == "") then do
                labelSetText label_cadastro_info ("Você digitou um nome vazio, tente novamente...")
            else do
                -- testa o tamanho do nome
                if ((length nome) > 30) then do
                    labelSetText label_cadastro_info ("Digite um nome de no máximo 30 caracteres!")
                else do
                    existe <- existe_nome nome
                    -- testa se nome já existe
                    if not (existe) then do
                        -- escreve no arquivo
                        escrever_arquivo nome
                        -- seta o texto do label
                        labelSetText label_cadastro_info ("\"" ++ nome ++ "\"" ++ " cadastrado com sucesso!")
                    else do
                        labelSetText label_cadastro_info ("Esse nome já existe! Escolha outro.")
