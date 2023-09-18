import System.IO
import Mapa

-- Tipos de dados para representar o mapa, cidades e estradas
type Nome = Mapa.Nome
type Coordenadas = (Double, Double)
type Estradas = [Mapa.Nome]
type Cidade = (Mapa.Nome, Coordenadas, Estradas)
type Mapa = [Mapa.Cidade]


-- Mapa inicial vazio
mapaInicial :: Mapa.Mapa
mapaInicial = []

-- Adiciona uma cidade ao mapa
addCidade :: Mapa.Mapa -> Mapa.Cidade -> Mapa.Mapa
addCidade mapa cidade = cidade : mapa

atualizarEstradasRemovidas :: Mapa.Mapa -> Mapa.Nome -> Mapa.Mapa
atualizarEstradasRemovidas [] _ = []
atualizarEstradasRemovidas ((nome, coordenadas, estradas):outrasCidades) cidadeRemovida =
    (nome, coordenadas, filter (/= cidadeRemovida) estradas) : atualizarEstradasRemovidas outrasCidades cidadeRemovida


-- Remove uma cidade do mapa, pelo nome
removeCidade :: Mapa.Mapa -> Mapa.Nome -> Mapa.Mapa
removeCidade mapa nomeCidade =
    let mapaAtualizado =  atualizarEstradasRemovidas mapa nomeCidade
    in [(nome, coordenadas, estradas) | (nome, coordenadas, estradas) <- mapaAtualizado, nome /= nomeCidade]


-- Adiciona uma estrada entre duas cidades
addEstrada :: Mapa.Mapa -> Mapa.Nome -> Mapa.Nome -> Mapa.Mapa
addEstrada mapa cidadeOrigem cidadeDestino =
    [(nome, coordenadas,atualizarEstradasAdicionadas nome estradas) |(nome, coordenadas, estradas) <- mapa]
    where
        atualizarEstradasAdicionadas nome estradas 
                                                    |nome == cidadeOrigem = estradas ++ [cidadeDestino]
                                                    |nome == cidadeDestino = estradas ++ [cidadeOrigem]
                                                    |otherwise = estradas

    
-- Remove uma estrada entre duas cidades
removeEstrada :: Mapa.Mapa -> Mapa.Nome -> Mapa.Nome -> Mapa.Mapa
removeEstrada mapa cidadeOrigem cidadeDestino =
    [(nome, coordenadas, if nome == cidadeOrigem then [estrada | estrada <- estradas, estrada /= cidadeDestino] else 
                        if nome == cidadeDestino then[estrada | estrada <- estradas, estrada /= cidadeDestino]  else estradas) |
    (nome, coordenadas, estradas) <- mapa]

-- Função auxiliar para verificar se uma cidade existe no mapa
cidadeExiste :: Mapa.Nome -> Mapa.Mapa -> Bool
cidadeExiste cidade mapa = elem cidade [nome | (nome, _, _) <- mapa]

-- Função para obter o nome de uma cidade do teclado, verificando se a cidade existe no mapa
checarCidade :: Mapa.Mapa -> IO Mapa.Nome
checarCidade mapa = do
    
    nome <- getLine
    if cidadeExiste nome mapa
        then return nome
        else do
            putStrLn "Cidade não encontrada. Tente novamente."
            checarCidade mapa

-- Função principal para manipular o mapa
manipularMapa :: Mapa.Mapa -> IO ()
manipularMapa mapa = do
    putStrLn "Mapa atual:"
    print mapa
    putStrLn "\nEscolha uma opção:"
    putStrLn "1 - Adicionar cidade"
    putStrLn "2 - Remover cidade"
    putStrLn "3 - Adicionar estrada"
    putStrLn "4 - Remover estrada"
    putStrLn "5 - Salvar mapa"
    putStrLn "6 - Carregar Mapa"
    putStrLn "7 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> do
            putStrLn "Digite o nome da cidade que você deseja adicionar:"
            nomeCidade <- getLine
    
            putStrLn "Digite as coordenadas (x,y) da cidade (separadas por espaço):"
            coordenadasStr <- getLine
            let [x, y] = map read (words coordenadasStr)
    
            let cidadeAdicionada = (nomeCidade, (x, y), [])
            manipularMapa (addCidade mapa cidadeAdicionada)
        "2" -> do
            putStrLn "Digite o nome da cidade a remover:"
            nome <- checarCidade mapa
            manipularMapa (removeCidade mapa nome)
        "3" -> do
            putStrLn "Digite o nome da cidade origem:"
            origem <- checarCidade mapa
            putStrLn "Digite o nome da cidade destino:"
            destino <- checarCidade mapa
            manipularMapa (addEstrada mapa origem destino)

        "4" -> do
            putStrLn "Digite o nome da cidade origem:"
            origem <- checarCidade mapa
            putStrLn "Digite o nome da cidade destino:"
            destino <- checarCidade mapa
            manipularMapa (removeEstrada mapa origem destino)


        "5" -> do 
            putStrLn "Salvando mapa..."
            salvouComSucesso <- salvarMapa mapa "cidades.mapa"
            if salvouComSucesso
                then manipularMapa mapa  -- Se salvou com sucesso, continua com o mapa atual
                else putStrLn "Erro ao salvar o mapa."  -- Caso contrário, exibe uma mensagem de erro

        "6" -> do
            putStrLn "Carregando mapa"
            mapa <- carregarMapa "cidades.mapa"
            manipularMapa mapa 
        "7" -> putStrLn "Saindo..."
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            manipularMapa mapa

-- Programa principal
main :: IO ()
main = do
    putStrLn "Bem-vindo ao sistema de mapas Fazuelly!"
    manipularMapa mapaInicial
