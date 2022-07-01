import Text.Show.Functions ()

--Punto 1
data Personaje = Personaje{
    edad                :: Int,
    energia             :: Int,
    habilidades         :: [String],
    nombre              :: String,
    planetaDeResidencia :: String
} deriving (Show,Eq)

type Gema = Personaje -> Personaje

data Guantelete = Guantelete{
    material :: String,
    gemas    :: [Gema]
} deriving (Show)

data Universo = Universo{
    habitantes :: [Personaje]
} deriving (Show,Eq)
--Podía haber sido Type Universo = [Personaje], pero pensaba que ajuro tenía que ser de tipo data

ironMan :: Personaje
ironMan = Personaje 40 50 ["Volar","Programar en Haskell"] "Tony" "La Tierra"

mapEdad :: (Int -> Int) -> Personaje -> Personaje
mapEdad f personaje = personaje{edad = f.edad $ personaje}


mapEnergia :: (Int -> Int) -> Personaje -> Personaje
mapEnergia f personaje = personaje{energia = f.energia $ personaje}

puedeHacerElChasquido :: Guantelete -> Bool
puedeHacerElChasquido guantelete = (( == "uru" ).material $ guantelete) && (( == 6 ).length.gemas $ guantelete)

cantPersonasUniverso :: Universo -> Int
cantPersonasUniverso universo = length.habitantes $ universo

mitadPersonasUniverso :: Universo -> Int
mitadPersonasUniverso universo = div (cantPersonasUniverso universo) 2

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo 
                            | puedeHacerElChasquido guantelete = universo{habitantes = take (mitadPersonasUniverso universo).habitantes $ universo}
                            | otherwise                        = universo

--Punto 2
esMenor :: Int -> (Personaje -> Int) -> (Personaje -> Bool)
esMenor edad funcion = (< edad).funcion

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex universo = any (esMenor 45 edad).habitantes $ universo

energiaTotal :: Universo -> Int
energiaTotal universo = sum.(map energia).habitantes $ universo


--Punto 3
restarEnergia :: Int -> Personaje -> Int
restarEnergia energiaRestada personaje = energia personaje - energiaRestada

laMente :: Int -> Gema
laMente energiaRestada personaje = personaje{energia = restarEnergia energiaRestada personaje}

poseeLaHabilidad :: String -> [String] -> Bool
poseeLaHabilidad habilidad listaHabilidades = elem habilidad listaHabilidades

eliminarHabilidad :: String -> [String] -> [String]
eliminarHabilidad habilidad listaHabilidades = filter (/= habilidad) listaHabilidades

elAlma :: String -> Gema
elAlma habilidad personaje = personaje{energia = restarEnergia 10 personaje,habilidades = eliminarHabilidad habilidad.habilidades $ personaje} --Falta lo de las habilidades

elEspacio :: String -> Gema
elEspacio nuevoPlaneta personaje = personaje{energia = restarEnergia 20 personaje, planetaDeResidencia = nuevoPlaneta}

tieneMenos :: Int -> [String] -> Bool --Repite Logica
tieneMenos cantidad personaje = (< cantidad).length $ personaje

tieneMenosDe2Habilidades :: [String] -> Bool
tieneMenosDe2Habilidades personaje = tieneMenos 2 personaje

quitarHabilidades :: [String] -> [String]
quitarHabilidades listaHabilidades
                                | tieneMenosDe2Habilidades listaHabilidades = []
                                | otherwise                                 = listaHabilidades

elPoder :: Gema
elPoder personaje = personaje{energia = 0,habilidades = quitarHabilidades.habilidades $ personaje}


rejuvenecer :: Int -> Int
rejuvenecer edad = max 18.div edad $ 2

elTiempo :: Gema
elTiempo personaje = personaje{energia = restarEnergia 50 personaje,edad = rejuvenecer.edad $ personaje}

laGemaLoca :: Gema -> Gema
laGemaLoca gemas rival = gemas.gemas $ rival

--Punto 4
unGuantelete :: Guantelete
unGuantelete = Guantelete "goma" [elTiempo, elAlma "usar Mjolnir",laGemaLoca $ elAlma "programacion en Haskell"]

--Punto 5
utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldr ($) enemigo gemas

--Punto 6
utilizarPorSeparado :: Personaje -> [Gema] -> [Personaje]
utilizarPorSeparado enemigo gemas = map ($ enemigo) gemas

gemaMasPoderosa :: Guantelete -> Personaje -> Int
gemaMasPoderosa guantelete victima = minimum.map energia.utilizarPorSeparado victima.gemas $ guantelete
--Obtiene el valor de energía mas bajo

--Punto 7
{-gemaMasPoderosa punisher guanteleteDeLocos: Estaría ejecutandose infinitamente, como el guantelete necesita evaluar el nivel de energia de la victima despues de usar
  cada gema, como este posee infinitas, nunca terminaría de evaluar -} 
{-usoLasTresPrimerasGemas guanteleteDeLocos punisher: Si se pueden usar las tres primeras gemas del guantele de Locos, ya que haskell funciona con Lazy Evaluation, por lo que
podría evaluar las primeras tres gemas sin importar que sea una lista infinita-}