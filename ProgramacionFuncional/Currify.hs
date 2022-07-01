import Text.Show.Functions ()

{-PARTE A-}
{-Estructuras-}
data Cancion = Cancion{
    titulo   :: String,
    genero   :: String,
    duracion :: Int
} deriving (Show,Eq)

data Artista = Artista{
    nombre         :: String,
    canciones      :: [Cancion],
    efectoFavorito :: Efecto
} deriving (Show)

type Efecto = Cancion -> Cancion
{-Estructuras-}

{-Mappers-}
mapDuracion :: (Int -> Int) -> Cancion -> Cancion
mapDuracion funcion cancion = cancion{duracion = funcion.duracion $ cancion}

mapTitulo :: (String -> String) -> Cancion -> Cancion
mapTitulo funcion cancion = cancion{titulo = funcion.titulo $ cancion}

mapGenero :: (String -> String) -> Cancion -> Cancion
mapGenero funcion cancion = cancion{genero = funcion.genero $ cancion}

mapCanciones :: ([Cancion] -> [Cancion]) -> Artista -> Artista
mapCanciones funcion artista = artista{canciones = funcion.canciones $ artista}
{-Mappers-}

{-Efectos-}
acortar :: Efecto
acortar cancion = mapDuracion (max 0.subtract 60) cancion

remixar :: Efecto
remixar cancion = mapGenero (const "remixado").mapDuracion (*2).mapTitulo (++ " remix") $ cancion

esDelGeneroAcustico :: String -> Bool
esDelGeneroAcustico generoDado = generoDado == "acústico"

acustizar :: Int -> Efecto
acustizar nuevaDuracion cancion 
                            | esDelGeneroAcustico.genero $ cancion = cancion
                            | otherwise                            = mapDuracion (const nuevaDuracion).mapGenero (const "acústico") $ cancion

metaEfecto :: [Efecto] -> Efecto
metaEfecto listaDeEfectos cancion = foldr ($) cancion listaDeEfectos
{-Efectos-}

cafeParaDos :: Cancion
cafeParaDos = Cancion "Cafe para dos" "melancolico" 146

fuiHastaAhi :: Cancion
fuiHastaAhi = Cancion "Fui hasta ahi" "rock" 279 

rocketRackoon :: Cancion
rocketRackoon = undefined

mientrasMiBateriaFesteja :: Cancion
mientrasMiBateriaFesteja = undefined

tomateDeMadera :: Cancion
tomateDeMadera = undefined

losEscarabajos :: Artista
losEscarabajos = Artista "Los Escarabajos" [rocketRackoon, mientrasMiBateriaFesteja, tomateDeMadera] acortar

teAcordas :: Cancion
teAcordas = undefined

unPibeComoVos :: Cancion
unPibeComoVos = undefined

daleMechaALaLluvia :: Cancion
daleMechaALaLluvia = undefined

adela :: Artista
adela = Artista "Adela" [cafeParaDos,fuiHastaAhi] remixar

elTigreJoaco :: Artista
elTigreJoaco = Artista "El Tigre Joaco" [] (acustizar 360)

{-PARTE B-}

vistazo :: Artista -> [Cancion]
vistazo artista = take 3.filter ((< 150).duracion).canciones $ artista

playList :: String -> [Artista] -> [Cancion]
playList generoBuscado listaDeArtistas = filter ((== generoBuscado).genero).concatMap canciones $ listaDeArtistas

{-PARTE C-}

hacerseDj :: Artista -> Artista
hacerseDj artista = mapCanciones (map (efectoFavorito artista)) artista

obtenerListaDe :: Eq a => (Cancion -> a) -> Artista -> [a]
obtenerListaDe funcion artista = map funcion.canciones $ artista

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo artista = all (== (head.obtenerListaDe genero $ artista)) (obtenerListaDe genero artista)

formarBanda :: String -> [Artista] -> Artista
formarBanda nombreBanda listaArtistas = Artista{
        nombre         = nombreBanda,
        canciones      = concatMap canciones listaArtistas,
        efectoFavorito = metaEfecto.map efectoFavorito $ listaArtistas
    }

generoConMasLetras :: String -> String -> String
generoConMasLetras genero1 genero2
                                | length genero1 < length genero2 = genero2
                                | otherwise                       = genero1

mejorGenero :: String -> String -> String
mejorGenero "rock" _                  = "rock"
mejorGenero _ "rock"                  = "rock"
mejorGenero "reggaeton" cualquierOtro = cualquierOtro
mejorGenero cualquierOtro "reggaeton" = cualquierOtro
mejorGenero genero1 genero2 = generoConMasLetras genero1 genero2

generoSuperador :: [String] -> String
generoSuperador listaGeneros = foldr1 mejorGenero listaGeneros ++ (" progresivo")

obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva artista = Cancion{
    titulo = (++ " progresivo") . concatMap titulo.canciones $ artista, 
    genero = generoSuperador.obtenerListaDe genero $ artista,
    duracion = foldl1 (+) (obtenerListaDe duracion artista)
}

{-PARTE D-}

-- Si
-- Si
-- Si