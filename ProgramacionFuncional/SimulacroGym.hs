data Persona = Persona{
    nombre           :: String,
    calorias         :: Int,
    indHidratacion   :: Int, {- 0/100 -}
    tiempoDisponible :: Int,
    equipamiento     :: [String]
}deriving (Show,Eq)

personaA :: Persona
personaA = Persona{nombre = "Carlos", calorias = 900, indHidratacion = 55, tiempoDisponible = 100, equipamiento = ["pesa"]}

personaE :: Persona
personaE = Persona{nombre = "Juan", calorias = 500, indHidratacion = 50, tiempoDisponible = 100, equipamiento = ["pesa"]}

personaI :: Persona
personaI = Persona{nombre = "Pedro", calorias = 100, indHidratacion = 100, tiempoDisponible = 200, equipamiento = ["pesa"]}

type ListaDePersonas = [Persona]
type Ejercicio = Persona -> Persona

listaPersona :: ListaDePersonas
listaPersona = [personaE, personaI]

{-Parte A-}

pierdeCalorias :: Int -> Int -> Persona -> Int
pierdeCalorias caloriasPerdidasPorRepeticion repeticiones persona = (calorias persona) - (repeticiones * caloriasPerdidasPorRepeticion)
--No vale la pena hacerlo por composicion, así es mas declarativa

pierdeIndHidratacion :: Int -> Int -> Persona -> Int
pierdeIndHidratacion repeticiones indPerdido persona = ((-) (indHidratacion persona)).(* indPerdido).(div repeticiones) $ 10

tienePesas :: Persona -> Bool
tienePesas persona = elem "pesa".equipamiento $ persona

abdominales :: Int -> Ejercicio
abdominales repeticiones persona = persona{calorias = pierdeCalorias 8 repeticiones persona}

flexiones :: Int -> Ejercicio
flexiones repeticiones persona = persona{calorias = pierdeCalorias 16 repeticiones persona, indHidratacion = pierdeIndHidratacion repeticiones 1 persona}

levantarPesas :: Int -> Int -> Ejercicio
levantarPesas repeticiones peso persona 
    | tienePesas persona = persona{calorias = pierdeCalorias 32 repeticiones persona, indHidratacion = pierdeIndHidratacion repeticiones peso persona}
    | otherwise          = laGranHomeroSimpson persona --No hace nada porque no tiene pesas

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson persona = persona

{-Parte 2A-}

renovarEquipo :: Persona -> Persona
renovarEquipo persona = persona{equipamiento = map ("nuevo " ++).equipamiento $ persona}

volverseYoguista :: Persona -> Persona
volverseYoguista persona = persona{calorias = (flip div 2).calorias $ persona, indHidratacion = min 100.(* 2).indHidratacion $ persona, equipamiento = ["colchoneta"]}

soloTienePesas :: Persona -> Bool
soloTienePesas persona = all (== "pesa").equipamiento $ persona

volverseBodyBuilder :: Persona -> Persona
volverseBodyBuilder persona
    | soloTienePesas persona = persona{nombre = (++ " BB").nombre $ persona, calorias = (* 3).calorias $ persona}
    | otherwise              = persona

comerUnSandwich :: Persona -> Persona
comerUnSandwich persona = persona{calorias = (+ 500).calorias $ persona, indHidratacion = 100}

{-Parte B-}

data Rutina = Rutina{
    ejercicios   :: [Ejercicio],
    duracionRutina     :: Int
}

miRutina :: [Ejercicio]
miRutina = [laGranHomeroSimpson,abdominales 10]

puedeHacerLaRutina :: Persona -> Rutina -> Bool
puedeHacerLaRutina persona rutina = (duracionRutina rutina <= tiempoDisponible persona)

hacerEjercicios :: a -> [(a -> a)] -> a 
hacerEjercicios unValor listaDeFunciones = foldr ($) unValor listaDeFunciones

hacerLaRutina :: Rutina -> Persona -> Persona
hacerLaRutina rutina persona = (hacerEjercicios persona).ejercicios $ rutina

peligro :: Int-> (Persona -> Int) -> Rutina -> Persona -> Bool
peligro menorQue funcion rutina persona= ((< menorQue).funcion.hacerLaRutina rutina $ persona)

esPeligrosa :: Persona -> Rutina -> Bool
esPeligrosa persona rutina = 
    puedeHacerLaRutina persona rutina    &&
    peligro 50 calorias rutina persona   && 
    peligro 10 calorias rutina persona  

divInverso :: Int -> Int -> Int
divInverso = flip div

mitadCaloriasInicias :: Persona -> Int
mitadCaloriasInicias persona = (divInverso 2).calorias $ persona

esBalanceada :: Persona -> Rutina -> Bool
esBalanceada persona rutina = 
    (puedeHacerLaRutina persona rutina)                                      &&
    ((> 10).indHidratacion.hacerLaRutina rutina $ persona)                   && 
    peligro (mitadCaloriasInicias persona) calorias rutina persona   

pam :: [(a -> a)] -> a -> [a] 
pam listaDeFunciones unValor = map ($ unValor) listaDeFunciones

elAbominableAbdominal :: Rutina
elAbominableAbdominal = Rutina{ejercicios = repeat (abdominales 1),duracionRutina = 60}

{-Parte C-}

seleccionarGrupoDeEjercicio :: Persona -> ListaDePersonas -> ListaDePersonas
seleccionarGrupoDeEjercicio persona listaPersona = filter (\x -> tiempoDisponible x == tiempoDisponible persona) listaPersona

promedio :: (Persona -> Int) -> ListaDePersonas -> Int
promedio funcion listaPersona = divInverso (length listaPersona).sum.map funcion $ listaPersona

promedioPostRutina :: ListaDePersonas -> Rutina -> (Persona -> Int) -> Int
promedioPostRutina listaPersona rutina funcion = promedio funcion.map (hacerLaRutina rutina) $ listaPersona 

promedioDeRutina :: ListaDePersonas -> Rutina -> (Int, Int)
promedioDeRutina listaPersona rutina = 
    (promedioPostRutina listaPersona rutina calorias, 
     promedioPostRutina listaPersona rutina indHidratacion)