import Text.Printf

type Nome = String
type Coordenadas = (Float, Float)
type Rotas = [Nome]
type Cidade = (Nome, Coordenadas, Rotas)
type Mapa = [Cidade]

meuFst :: (a,b,c) -> a
meuFst (a,_,_) = a

meuTrd :: (a,b,c) -> c
meuTrd(_,_,c) = c
mapaInicial :: Mapa
mapaInicial = []

addCidade :: Mapa -> Cidade -> Mapa
addCidade mapa cidade = cidade : mapa

removeCidade :: Mapa -> Nome -> Mapa
removeCidade mapa nomeCidade = [(nome, coordenadas, rotas) | (nome, coordenadas, rotas) <- mapa, nome /= nomeCidade]


-- Adicionei as rotas
addRotas :: Mapa -> Nome -> Nome -> Mapa
addRotas mapa cidadeOrigem cidadeDestino =
    let mapaAtualizado = [(nome, coords, if nome == cidadeOrigem then (meuTrd (nome, coords, rotas)) ++ [cidadeDestino] else rotas) | (nome, coords, rotas) <- mapa]
    in mapaAtualizado

-- Removi as rotas
removeRotas :: Mapa -> Nome -> Nome -> Mapa
removeRotas mapa cidadeOrigem cidadeDestino =
    let mapaAtualizado = [(nome, coords, if nome == cidadeOrigem then [rota | rota <- rotas, rota /= cidadeDestino] else rotas) | (nome, coords, rotas) <- mapa]
    in mapaAtualizado

main :: IO ()
main = do
    putStrLn "Mapa inicial:"
    print mapaInicial

    let cidade1 = ("Aracaju", (1.0, 2.0), [])
    printf "Adicionando %s ao mapa\n" (meuFst cidade1)
    let mapa1 = addCidade mapaInicial cidade1
    print mapa1

    let cidade2 = ("Itabaiana", (3.0, 4.0), [])
    printf "Adicionando %s ao mapa\n" (meuFst cidade2)
    let mapa2 = addCidade mapa1 cidade2
    print mapa2

-- Adicionei as rotas
    let mapaComRotas = addRotas mapa2 "Aracaju" "Itabaiana"
    printf "Adicionando rota entre Aracaju e Itabaiana\n"
    print mapaComRotas

-- Removi as rotas
    let mapaSemRotas = removeRotas mapaComRotas "Aracaju" "Itabaiana"
    printf "Removendo rota entre Aracaju e Itabaiana\n"
    print mapaSemRotas

    let cidade3 = ("Lagarto", (5.0, 6.0), [])
    printf "Adicionando %s ao mapa\n" (meuFst cidade3)
    let mapa3 = addCidade mapa2 cidade3
    print mapa3

    printf "Removendo %s do mapa\n" (meuFst cidade1)
    let mapa4 = removeCidade mapa3 "Aracaju"
    print mapa4

    printf "Removendo %s do mapa\n" (meuFst cidade2)
    let mapa5 = removeCidade mapa4 "Itabaiana"
    print mapa5

    printf "Removendo %s do mapa\n" (meuFst cidade3)
    let mapaFinal = removeCidade mapa5 "Lagarto"
    print mapaFinal
