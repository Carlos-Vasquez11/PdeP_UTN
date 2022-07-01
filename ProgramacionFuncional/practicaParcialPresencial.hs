{-Parte A-}
type Titulo = String
type Genero = String
type Duracion = Int

type Cancion = (Titulo, Genero, Duracion) 

type Efecto = (String, Int)

data Artista = Artista{
  nombre :: String,
  canciones :: [Cancion],
  efectoFavorito :: [ ( String, Int ) ]
} deriving (Show,Eq)

--Canciones:
cafeParaDos :: Cancion
cafeParaDos = ("Cafe para dos","Rock",146)

fuiHastaAhi :: Cancion
fuiHastaAhi = ("Fui hasta ahí", "Rock", 279)

--Artistas:
losEscarabajos :: Artista
losEscarabajos = 
    Artista{nombre= "Los Escarabajos", 
    canciones = [
        ("Rocket Rackoon",              "Racki", 120),
        ("Mientras mi batería festeja", "Rack suave", 120),
        ("Tomate de madera.",           "Racka", 150)],
    efectoFavorito = [("acortar",0)]}

adela :: Artista
adela = Artista{nombre= "Adela", 
canciones = [
        (" ¿Te acordás?",          "Balada", 120),
        ("Un pibe como vos",       "Heavy Metal", 170),
        ("Dale mecha a la lluvia", "Country", 120)], 
    efectoFavorito = [("remixar",0)]}

elTigreJoaco :: Artista
elTigreJoaco = Artista{nombre= "El Tigre Joaco", 
    canciones = [], 
    efectoFavorito = [("acustizar",6)]}


{-Parte B-}

duraMenosDe150s :: Cancion -> Bool
duraMenosDe150s (_,_,duracion) = duracion < 150

vistazo :: Artista -> [Cancion]
vistazo unArtista = take 3.filter duraMenosDe150s.canciones $ unArtista

perteneceAlGenero :: String -> Cancion -> Bool
perteneceAlGenero genero (_,elGenero,_)= (genero == elGenero)

auxiliar :: String -> Artista -> [Cancion]
auxiliar genero unArtista = filter (perteneceAlGenero genero) (canciones $ unArtista)

playList :: String -> [Artista] -> [Cancion]
playList genero listaDeArtistas = concat.(map (auxiliar genero)) $ listaDeArtistas


{-Parte C-}

devolverNombre :: Cancion -> String
devolverNombre (nombre,_ , _) = nombre

devolverTiempo :: Cancion -> Int
devolverTiempo (_, _, tiempo) = tiempo

devolverGenero :: Cancion -> String
devolverGenero (_, genero, _) = genero

modificarTiempo :: Cancion -> Cancion
modificarTiempo (nombre, genero, tiempo) = (nombre,genero , tiempo + 1)

remixar :: Cancion -> Cancion
remixar (nombre, _, tiempo) = (nombre ++ " remix", "remixado", tiempo * 2)

acustizar :: Cancion -> Cancion
acustizar (nombre, "acústico", tiempo) = (nombre, "acústico", tiempo)
acustizar (nombre, _, tiempo) = (nombre, "acústico", tiempo + 6)

aplicarEfectos ::  [Cancion] -> Efecto -> [Cancion]
aplicarEfectos listaCanciones ("acortar", _) = map (modificarTiempo) listaCanciones

aplicarEfectos listaCanciones ("remixar", _) = map (remixar) listaCanciones

aplicarEfectos listaCanciones ("acustizar", duracion) = map (acustizar) listaCanciones

devolverListaCanciones :: Artista -> [Cancion]
devolverListaCanciones unArtista = canciones $ unArtista

devolverListaEfectos :: Artista -> [Efecto]
devolverListaEfectos unArtista = efectoFavorito $ unArtista

--hacerseDJ :: Artista -> Artista
--hacerseDJ unArtista = unArtista{canciones = map (aplicarEfectos (devolverListaCanciones unArtista)) (devolverListaEfectos unArtista)}
--Ya tengo la funcion de aplicar efectos que dado un efecto se lo aplico a una lista de canciones. Ahora necesito que para cada efecto
--dentro de efecto favorito se aplique esa funcion 

tieneGustoHomogeneo :: String -> Artista -> Bool
tieneGustoHomogeneo genero unArtista = all (perteneceAlGenero genero) (devolverListaCanciones unArtista)

concatenarTodo :: (Artista -> [a]) -> [Artista] -> [a]
concatenarTodo funcion listaDeArtistas = concat.(map funcion) $ listaDeArtistas

formarBanda :: String -> [Artista] -> Artista
formarBanda nombreBanda listaDeArtistas = Artista{nombre = nombreBanda, 
                                                  canciones =      concatenarTodo devolverListaCanciones listaDeArtistas,
                                                  efectoFavorito = concatenarTodo devolverListaEfectos listaDeArtistas
                                                  }

concatenarNombres :: Artista -> String
concatenarNombres unArtista = concat(map devolverNombre (devolverListaCanciones unArtista))

sumarTiempos :: Artista -> Int
sumarTiempos unArtista = sum(map devolverTiempo (devolverListaCanciones unArtista))

devolverListaGeneros :: [Cancion] -> [String]
devolverListaGeneros listaCanciones = map devolverGenero listaCanciones

quitarReggaeton :: [String] -> [String]
quitarReggaeton listaGeneros = filter (/= "reggaeton") listaGeneros

generoSuperior :: [String] -> String
generoSuperior listaGeneros
                            | any (== "Rock") listaGeneros = "Rock"
                            | otherwise = foldl1 max listaGeneros --No Funciona

obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = (concatenarNombres unArtista, generoSuperior.devolverListaGeneros.devolverListaCanciones $ unArtista,
                                   sumarTiempos unArtista)

{-Parte D-}
{-
1. No puede hacerse DJ, ya que como tiene infinitas canciones nunca terminaría de aplicar el efecto a todas
2. Si podemos, ya que se terminaría la ejecución al encontrar las primeras 3 canciones que duren menos de 2 minutos y medio.
Aunque por como yo codifique la funcion "vistazo" quedaría ejecutandose infinitamente :S
3. No podra hacerlo, ya que el titulo de la obra maestra es la concatenacion de todos los titulos de sus anteriores canciones, como
la lista es infinita estará concatenando infinitamente
-}