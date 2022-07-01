{-PARTE A-}
data Persona = Persona{
    nombre           :: String,
    calorias         :: Int,
    indHidratacion   :: Int, {-0 - 100-}
    tiempoDisponible :: Int,
    herramientas     :: [String]
}

type Ejercicio = Persona -> Persona

carlos :: Persona
carlos = Persona "Carlos" 100 50 30 ["Pesa"]

{-Mappers-}
mapCalorias :: (Int -> Int) -> Persona -> Persona
mapCalorias funcion persona = persona{calorias = funcion.calorias $ persona}

mapIndHidratacion :: (Int -> Int) -> Persona -> Persona
mapIndHidratacion funcion persona = persona{indHidratacion = max 100.funcion.indHidratacion $ persona}

mapHerramientas :: ([String] -> [String]) -> Persona -> Persona
mapHerramientas funcion persona = persona{herramientas = funcion.herramientas $ persona}

setHerramientas :: String -> Persona -> Persona
setHerramientas herramienta persona = persona{herramientas = [herramienta]}

divInverso :: Int -> Int -> Int
divInverso = flip div

{-Mappers-}
perderCalorias :: Int -> Int -> Persona -> Persona
perderCalorias caloriasPerdidas repeticiones persona = mapCalorias (subtract (caloriasPerdidas * repeticiones)) persona

deshidratacion :: Int -> Int -> Persona -> Persona
deshidratacion indHidratacionPerdida repeticiones persona = mapIndHidratacion (subtract ((div repeticiones 10) * indHidratacionPerdida )) persona

abdominales :: Int -> Ejercicio
abdominales repeticiones persona = perderCalorias 8 repeticiones persona

flexiones :: Int -> Ejercicio
flexiones repeticiones persona = deshidratacion 2 repeticiones . perderCalorias 16 repeticiones $ persona

tienePesas :: Persona -> Bool
tienePesas persona = elem "pesa" . herramientas $ persona

levantarPesas :: Int -> Int -> Ejercicio
levantarPesas repeticiones peso persona 
                                    | tienePesas persona = deshidratacion peso repeticiones . perderCalorias 32 repeticiones $ persona
                                    | otherwise          = persona

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson persona = persona

{-PARTE A.2-}

renovarEquipo :: Ejercicio
renovarEquipo persona = mapHerramientas (map ("Nuevo " ++)) persona

volverseYoguista :: Ejercicio
volverseYoguista persona = setHerramientas "colchoneta" . mapIndHidratacion (* 2) . mapCalorias (divInverso 2) $ persona

soloTienePesas :: Persona -> Bool
soloTienePesas persona = all (== "pesa").herramientas $ persona --Chequear

volverseBodyBuilder :: Ejercicio
volverseBodyBuilder persona 
                            | soloTienePesas persona = mapCalorias (* 2) $ persona{nombre = (++ " BB").nombre $ persona}
                            | otherwise              = persona

comerUnSandwich :: Ejercicio
comerUnSandwich persona = mapIndHidratacion (const 100) . mapCalorias (+ 500) $ persona

{-PARTE B-}

data Rutina = Rutina{
    ejercicios :: [Ejercicio],
    duracion   :: Int
}

puedeHacerLaRutina :: Rutina -> Persona -> Bool
puedeHacerLaRutina rutina persona = tiempoDisponible persona >= duracion rutina

hacerLaRutina :: Rutina -> Persona -> Persona
hacerLaRutina rutina persona
                            | puedeHacerLaRutina rutina persona = foldr ($) persona . ejercicios $ rutina
                            | otherwise                         = persona

chequearPersona :: (Persona -> Int) -> (Int -> Bool) -> Rutina -> Persona -> Bool
chequearPersona caracteristica comparaConValorClave rutina persona = comparaConValorClave. caracteristica . hacerLaRutina rutina $ persona

esPeligrosa :: Rutina -> Persona -> Bool
esPeligrosa rutina persona = chequearPersona calorias (< 50) rutina persona && chequearPersona indHidratacion (< 10) rutina persona

esBalanceada :: Rutina -> Persona -> Bool
esBalanceada rutina persona = chequearPersona indHidratacion (> 80) rutina persona && chequearPersona calorias (< div (calorias persona) 2) rutina persona

elAbominableAbdominal :: Rutina
elAbominableAbdominal = Rutina{ejercicios = map abdominales [1..] , duracion = 60}

{-PARTE C-}

tienenElMismoTiempoLibre :: Persona -> (Persona -> Bool)
tienenElMismoTiempoLibre persona = (== tiempoDisponible persona).tiempoDisponible

seleccionarGrupoDeEjercicio :: Persona -> [Persona] -> [Persona]
seleccionarGrupoDeEjercicio persona listaDePersonas = filter (tienenElMismoTiempoLibre persona) listaDePersonas

promedioDe :: (Persona -> Int) -> Rutina -> [Persona] -> Int
promedioDe caracteristica rutina listaDePersonas = divInverso (length listaDePersonas) . sum . map caracteristica . map (hacerLaRutina rutina) $ listaDePersonas

promedioDeRutina :: Rutina -> [Persona] -> (Int,Int)
promedioDeRutina rutina listaDePersonas = (promedioDe calorias rutina listaDePersonas, promedioDe indHidratacion rutina listaDePersonas)