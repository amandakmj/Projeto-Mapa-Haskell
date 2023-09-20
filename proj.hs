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
addCidade mapa cidade =  cidade : mapa 

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
                        if nome == cidadeDestino then[estrada | estrada <- estradas, estrada /= cidadeOrigem]  else estradas) |
    (nome, coordenadas, estradas) <- mapa]

-- Função auxiliar para verificar se uma cidade existe no mapa
cidadeExiste :: Mapa.Nome -> Mapa.Mapa -> Bool
cidadeExiste cidade mapa = elem cidade [nome | (nome, _, _) <- mapa]

coordenadasExistem :: Coordenadas -> Mapa.Mapa -> Bool
coordenadasExistem coordenadas mapa = elem coordenadas [(x, y) | (_, (x, y), _) <- mapa]

-- Verifica se duas cidades possuem conexão
estradaExiste :: Mapa.Mapa -> Mapa.Nome -> Mapa.Nome -> Bool
estradaExiste mapa cidadeOrigem cidadeDestino =
    let estradasOrigem = head [estradas | (nome, _, estradas) <- mapa, nome == cidadeOrigem]
        estradasDestino = head [estradas | (nome, _, estradas) <- mapa, nome == cidadeDestino]
    in cidadeDestino `elem` estradasOrigem || cidadeOrigem `elem` estradasDestino


-- Função para obter o nome de uma cidade do teclado, verificando se a cidade existe no mapa
checarCidade :: Mapa.Mapa -> IO Mapa.Nome
checarCidade mapa = do
    
    nome <- getLine
    if cidadeExiste nome mapa
        then return nome
        else do
            putStrLn "Cidade não encontrada. Tente novamente."
            checarCidade mapa

--Função que checa se a cidade que o usuário digitar ja existe no mapa,de maneira que não possam haver mais de uma cidade com o mesmo nome
checarCidadeRepetida :: Mapa.Mapa -> IO Mapa.Nome 
checarCidadeRepetida  mapa= do
    
    nome <- getLine        
    if cidadeExiste nome mapa == False
        then return nome
         else do
            putStrLn "Esta cidade já existe. Tente novamente."
            checarCidadeRepetida mapa
--Função que checa se as coordenadas digitadas pelo usuário já fazem parte de uma cidade existente, pois duas cidades não podem ocupar o mesmo espaço
checarCoordenadasRepetidas :: Mapa.Mapa -> IO Coordenadas
checarCoordenadasRepetidas mapa = do
    coordenadasStr <- getLine
    let [x, y] = map read (words coordenadasStr)
    if coordenadasExistem (x, y) mapa
        then do
            putStrLn "Essas coordenadas já existem. Tente novamente."
            checarCoordenadasRepetidas mapa
        else return (x, y)


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
            nomeCidade <- checarCidadeRepetida mapa
    
            putStrLn "Digite as coordenadas (x,y) da cidade (separadas por espaço):"
            (x, y) <- checarCoordenadasRepetidas mapa
    
            let cidadeAdicionada = (nomeCidade, (x, y), [])
            manipularMapa (addCidade mapa cidadeAdicionada)
        "2" ->
            if length mapa == 0 
                then do
                    putStrLn "Nao eh possivel remover uma cidade de um mapa vazio."
                    manipularMapa mapa
            else do
                putStrLn "Digite o nome da cidade a remover:"
                nome <- checarCidade mapa
                manipularMapa (removeCidade mapa nome)
        "3" -> 
            if length mapa == 0 
                then do
                    putStrLn "Nao eh possivel adicionar uma estrada a um mapa vazio."
                    manipularMapa mapa
            else do
            putStrLn "Digite o nome da cidade origem:"
            origem <- checarCidade mapa
            putStrLn "Digite o nome da cidade destino:"
            destino <- checarCidade mapa
            manipularMapa (addEstrada mapa origem destino)

        "4" -> 
            if length mapa == 0 
                then do
                    putStrLn "Nao eh possivel remover uma estrada de um mapa vazio."
                    manipularMapa mapa
            else do        
            putStrLn "Digite o nome da cidade origem:"
            origem <- checarCidade mapa
            putStrLn "Digite o nome da cidade destino:"
            destino <- checarCidade mapa
            if estradaExiste mapa origem destino
                then do
                    manipularMapa (removeEstrada mapa origem destino)
            else do
                putStrLn "As cidades estao desconectadas"
                manipularMapa mapa


        "5" -> do 
            putStrLn "Digite o nome do mapa que voce deseja salvar(com a extensao.mapa)."
            nomeMapa <- getLine
            putStrLn "Salvando mapa..."
            salvouComSucesso <- salvarMapa mapa nomeMapa
            if salvouComSucesso
                then manipularMapa mapa  -- Se salvou com sucesso, continua com o mapa atual
                else putStrLn "Erro ao salvar o mapa."  -- Caso contrário, exibe uma mensagem de erro

        "6" -> do
            putStrLn "Digite o nome do mapa que voce deseja carregar(com a extensao.mapa)."
            nomeMapa <- getLine
            putStrLn "Carregando mapa"
            mapa <- carregarMapa nomeMapa
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
