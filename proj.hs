import System.IO
import Mapa

-- Tipos de dados para representar o mapa, cidades e estradas
type Nome = Mapa.Nome
type Coordenadas = (Double, Double)
type Estradas = [Mapa.Nome]
type Cidade = (Mapa.Nome, Coordenadas, Estradas)
type Mapa = [Mapa.Cidade]

-- pega o nome da cidade
getNome :: Mapa.Cidade -> Mapa.Nome
getNome (nome,coordenadas,estradas) = nome
--pega as coordenadas da cidade
getCoords :: Mapa.Cidade -> Coordenadas
getCoords (_,coordenadas,_) =coordenadas
-- pega todos os dados da cidade a partir do nome
getCidade :: Mapa.Nome -> Mapa.Mapa -> Mapa.Cidade
getCidade nome mapa = head [cidade | cidade <- mapa,nome == getNome cidade]
--pega as estradas da cidade recebendo apenas o nome
getEstradas :: Mapa.Nome -> Mapa.Mapa -> Estradas
getEstradas nome mapa = getEstradasCidade $ getCidade nome mapa
                            where
                                getEstradasCidade (_,_,estradas) = estradas


-- Mapa inicial vazio
mapaInicial :: Mapa.Mapa
mapaInicial = []
 
mapaTeste :: Mapa.Mapa
mapaTeste = [("Queimadas",(5.0,6.0),["Boqueirao","Pombal"]),("Boqueirao",(4.0,5.0),["Joao Pessoa","Queimadas"]),("Pombal",(12.0,32.0),
            ["Campina Grande","Queimadas"]),("Joao Pessoa",(4.0,7.0),["Campina Grande","Boqueirao"]),
            ("Campina Grande",(9.0,0.0),["Joao Pessoa","Pombal"])]
-- Adiciona uma cidade ao mapa
addCidade :: Mapa.Mapa -> Mapa.Cidade -> Mapa.Mapa
addCidade mapa (nome,coordenadas,estradas) =  (nome,coordenadas,[]) : mapa 

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
                                                    |cidadeOrigem == cidadeDestino = estradas
                                                    |nome == cidadeOrigem = estradas ++ [cidadeDestino]
                                                    |nome == cidadeDestino = estradas ++ [cidadeOrigem]
                                                    |otherwise = estradas

    
-- Remove uma estrada entre duas cidades
removeEstrada :: Mapa.Mapa -> Mapa.Nome -> Mapa.Nome -> Mapa.Mapa
removeEstrada mapa cidadeOrigem cidadeDestino =
    [(nome, coordenadas, if nome == cidadeOrigem then [estrada | estrada <- estradas, estrada /= cidadeDestino] else 
                        if nome == cidadeDestino then[estrada | estrada <- estradas, estrada /= cidadeOrigem]  else estradas) |
    (nome, coordenadas, estradas) <- mapa]

calcularDistancia :: Mapa.Nome -> Mapa.Nome -> Mapa.Mapa -> Double
calcularDistancia cidadeOrigem cidadeDestino mapa = sqrt((x1 - x2)^2 + (y1 - y2)^2)
                                                    where 
                                                        (x1, y1) = getCoords $ getCidade cidadeOrigem mapa
                                                        (x2, y2) = getCoords $ getCidade cidadeDestino mapa



-- Função auxiliar para verificar se uma cidade existe no mapa
cidadeExiste :: Mapa.Nome -> Mapa.Mapa -> Bool
cidadeExiste cidade mapa = elem cidade [nome | (nome, _, _) <- mapa]

coordenadasExistem :: Coordenadas -> Mapa.Mapa -> Bool
coordenadasExistem coordenadas mapa = elem coordenadas [(x, y) | (_, (x, y), _) <- mapa]

-- Verifica se duas cidades possuem conexão
estradaExiste :: Mapa.Mapa -> Mapa.Nome -> Mapa.Nome -> Bool
estradaExiste mapa cidadeOrigem cidadeDestino =
    cidadeDestino `elem` estradasOrigem && cidadeOrigem `elem` estradasDestino
    where
        estradasOrigem = getEstradas cidadeOrigem  mapa
        estradasDestino = getEstradas cidadeDestino  mapa

  

rotaExiste :: Mapa.Mapa -> Mapa.Nome -> Mapa.Nome -> Bool
rotaExiste mapa cidadeOrigem cidadeDestino 
    | null estradasOrigem || null estradasDestino = False
    | cidadeOrigem == cidadeDestino = True
    | existeConexaoDireta = True
    | otherwise = any (\cidade -> rotaExiste mapa cidade cidadeDestino) cidadesIntermediarias
    where
        estradasOrigem = getEstradas cidadeOrigem mapa
        estradasDestino = getEstradas cidadeDestino mapa
        cidadesIntermediarias = filter (\cidade -> cidade /= cidadeOrigem && cidade /= cidadeDestino) estradasOrigem
        existeConexaoDireta = cidadeDestino `elem` estradasOrigem


-- Função para encontrar um caminho entre duas cidades
encontrarCaminho :: Mapa.Mapa -> Mapa.Nome -> Mapa.Nome -> [Mapa.Nome]
encontrarCaminho mapa cidadeOrigem cidadeDestino = buscarCaminho cidadeOrigem cidadeDestino []
  where
    buscarCaminho atual destino caminho
        | atual == destino = destino : caminho
        | otherwise =
            let estradas = getEstradas atual mapa
                cidadesVizinhas = filter (\cidade -> cidade `notElem` caminho) estradas
            in case filter (\cidade -> rotaExiste mapa cidade destino) cidadesVizinhas of
                [] -> []
                (proximaCidade : _) -> buscarCaminho proximaCidade destino (atual : caminho)

calcularSomaDistancias :: Mapa.Mapa -> [Mapa.Nome] -> Double
calcularSomaDistancias _ [] = 0.0
calcularSomaDistancias mapa [_] = 0.0
calcularSomaDistancias mapa (cidade1:cidade2:cidadesRestantes) =
                    calcularDistancia cidade1 cidade2 mapa + calcularSomaDistancias mapa (cidade2:cidadesRestantes)

calcularDistanciaRota :: Mapa.Mapa -> Mapa.Nome -> Mapa.Nome -> Double
calcularDistanciaRota mapa origem destino 
                                         |rotaExiste mapa origem destino = calcularSomaDistancias mapa caminho
                                         |otherwise = error "nao ha rota valida"
                                         where
                                            caminho = encontrarCaminho mapa origem destino




                                                


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

checarEstradaRepetida :: Mapa.Mapa -> Mapa.Nome -> Mapa.Nome -> IO (Mapa.Nome, Mapa.Nome)
checarEstradaRepetida mapa origem destino = do
    if estradaExiste mapa origem destino
        then do
            putStrLn "Essa estrada já existe. Tente novamente."
            putStrLn "Digite novamente a cidade origem:"
            origemNova <- checarCidade mapa
            putStrLn "Digite novamente a cidade destino:"
            destinoNova <- checarCidade mapa
            checarEstradaRepetida mapa origemNova destinoNova
        else return (origem, destino)

-- Função que retorna os nomes das cidades conectadas a uma cidade por uma estrada
getCidadesConectadas :: Mapa.Mapa -> Mapa.Nome -> [Mapa.Nome]
getCidadesConectadas mapa cidadeOrigem =
    case getCidade cidadeOrigem mapa of
        (_, _, estradas) -> estradas

-- Função que mostra as cidades em uma rota entre duas cidades
mostrarRotaEntreCidades :: Mapa.Mapa -> Mapa.Nome -> Mapa.Nome -> IO ()
mostrarRotaEntreCidades mapa cidadeOrigem cidadeDestino =
    case encontrarCaminho mapa cidadeOrigem cidadeDestino of
        [] -> putStrLn "Não há rota entre as cidades."
        rota -> putStrLn $ "Rota entre " ++ cidadeOrigem ++ " e " ++ cidadeDestino ++ ": " ++ unwords rota


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
    putStrLn "5 - Calcular distância entre duas cidades (pela rota)"
    putStrLn "6 - Verificar se existe rota entre duas cidades"
    putStrLn "7 - Mostra as cidades conectadas a uma cidade"
    putStrLn "8 - Mostra a rota entre duas cidades"
    putStrLn "9 - Salvar mapa"
    putStrLn "10 - Carregar mapa"
    putStrLn "11 - Sair"
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
                if null mapa
                    then do
                        putStrLn "Não é possivel realizar essa operação num mapa vazio,por favor adicione cidades ou carregue um mapa para prosseguir"
                        manipularMapa mapa
                else do
                    putStrLn "Digite o nome da cidade a remover:"
                    nome <- checarCidade mapa
                    manipularMapa (removeCidade mapa nome)
        "3" -> 
            if null mapa
                then do
                    putStrLn "Não é possivel realizar essa operação num mapa vazio,por favor adicione cidades ou carregue um mapa para prosseguir"
                    manipularMapa mapa
            else do
                 putStrLn "Digite o nome da cidade origem:"
                 origem <- checarCidade mapa
                 putStrLn "Digite o nome da cidade destino:"
                 destino <- checarCidade mapa
                 (origem, destino) <- checarEstradaRepetida mapa origem destino
                 manipularMapa (addEstrada mapa origem destino)


        "4" -> 
            if null mapa
                then do
                    putStrLn "Não é possivel realizar essa operação num mapa vazio,por favor adicione cidades ou carregue um mapa para prosseguir"
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

        "5" -> 
            if null mapa
                then do
                    putStrLn "Não é possivel realizar essa operação num mapa vazio,por favor adicione cidades ou carregue um mapa para prosseguir"
                    manipularMapa mapa
            else do
                putStrLn "Digite o nome da cidade origem"
                origem <- checarCidade mapa
                putStrLn "Digite o nome da cidade destino:"
                destino <- checarCidade mapa
                if rotaExiste mapa origem destino
                    then do
                        let distancia = calcularDistanciaRota mapa origem destino 
                        putStrLn $ "A distância entre " ++ origem ++ " e " ++ destino ++ " é: " ++ show distancia
                        manipularMapa mapa
                else do
                    putStrLn "As cidades nao possuem rota."
                    manipularMapa mapa
            
        "6" -> do
            putStrLn "Digite o nome da cidade origem"
            origem <- checarCidade mapa
            putStrLn "Digite o nome da cidade destino:"
            destino <- checarCidade mapa
            if rotaExiste mapa origem destino
                then
                    putStrLn "As cidades possuem uma ou mais rotas."
                    else
                        putStrLn "As cidades não possuem rotas."
            manipularMapa mapa
        "7" -> do
            putStrLn "Digite o nome da cidade:"
            cidade <- checarCidade mapa
            let cidadesConectadas = getCidadesConectadas mapa cidade
            putStrLn $ "Cidades conectadas a " ++ cidade ++ ": " ++ unwords cidadesConectadas
            manipularMapa mapa
            
        "8" -> do
            putStrLn "Digite o nome da cidade origem:"
            origem <- checarCidade mapa
            putStrLn "Digite o nome da cidade destino:"
            destino <- checarCidade mapa
            mostrarRotaEntreCidades mapa origem destino
            manipularMapa mapa

        "9" -> do 
            putStrLn "Digite o nome do mapa que voce deseja salvar(com a extensao.mapa)."
            nomeMapa <- getLine
            putStrLn "Salvando mapa..."
            salvouComSucesso <- salvarMapa mapa nomeMapa
            if salvouComSucesso
                then manipularMapa mapa  -- Se salvou com sucesso, continua com o mapa atual
                else putStrLn "Erro ao salvar o mapa."  -- Caso contrário, exibe uma mensagem de erro

        "10" -> do
            putStrLn "Digite o nome do mapa que voce deseja carregar(com a extensao.mapa)."
            nomeMapa <- getLine
            putStrLn "Carregando mapa"
            mapa <- carregarMapa nomeMapa
            manipularMapa mapa 

        "11" -> putStrLn "Saindo..."
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            manipularMapa mapa

-- Programa principal
main :: IO ()
main = do
    putStrLn "Bem-vindo ao sistema de mapas Fazuelly!"
    manipularMapa mapaInicial