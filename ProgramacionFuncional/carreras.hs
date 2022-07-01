data Auto = Auto{
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show,Eq)

type CarreraAutos = [Auto]

sonAutosDistintos :: Auto -> Auto -> Bool
sonAutosDistintos auto1 auto2 = color auto1 /= color auto2
-- Se puede componer como: (/= color auto2).color $ auto1 pero en mi opinion pierde declaratividad

autop1 ::Auto
autop1 = Auto{color = "rojo",velocidad = 45, distancia = 15}
autop2 ::Auto
autop2 = Auto{color = "verde",velocidad = 50, distancia = 1}
autop3 ::Auto
autop3 = Auto{color = "azul",velocidad = 50, distancia = 10}

carrera :: CarreraAutos
carrera = [autop1,autop2,autop3]

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = (&& sonAutosDistintos auto1 auto2).(< 10).abs.subtract (distancia auto1).distancia $ auto2

vaTranquilo :: Auto -> CarreraAutos -> Bool
vaTranquilo auto carrera = (not.any (estaCerca auto)) carrera && lesVaGanandoATodos auto carrera

lesVaGanandoATodos :: Auto -> CarreraAutos -> Bool
lesVaGanandoATodos auto1 carrera = all (leVaGanando auto1) carrera

leVaGanando :: Auto -> Auto -> Bool
leVaGanando auto1 auto2 = (distancia auto1 > distancia auto2) && (color auto1 /= color auto2)

leEstaGanandoYEsDistinto :: Auto -> Auto -> Bool
leEstaGanandoYEsDistinto auto1 auto2 = (not.leVaGanando auto1) auto2 && (color auto1 /= color auto2)

puesto :: Auto -> CarreraAutos -> Int
puesto auto carrera = (+ 1).length.filter (leEstaGanandoYEsDistinto auto) $ carrera

--2.
correr auto tiempo = tiempo * velocidad auto

corra :: Int -> Auto -> Auto
corra tiempo auto = auto{distancia = (+ correr auto tiempo).distancia $ auto}

modificadorVelocidad :: Auto -> (Int -> Int) -> Auto
modificadorVelocidad auto funcion = auto{velocidad = funcion.velocidad $ auto}

bajarLaVelocidad :: Int -> Auto -> Auto
bajarLaVelocidad velocidadReducida auto = modificadorVelocidad auto (max 0.subtract velocidadReducida)

--3.
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista 
    = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type PowerUp = Auto -> CarreraAutos -> CarreraAutos

terremoto :: PowerUp 
terremoto auto carrera = afectarALosQueCumplen (estaCerca auto) (bajarLaVelocidad $ 50) carrera

miguelito :: Int -> PowerUp
miguelito velocidadRestada auto carrera = afectarALosQueCumplen (leVaGanando auto) (bajarLaVelocidad $ velocidadRestada) carrera

duplicarVelocidad :: Auto -> Auto
duplicarVelocidad = flip modificadorVelocidad (*2)


jetPack :: Int -> PowerUp
jetPack tiempo auto carrera = afectarALosQueCumplen (== auto) (corra tiempo.duplicarVelocidad) carrera

--4.

type Evento = CarreraAutos -> CarreraAutos
correnTodos :: Int -> Evento
correnTodos tiempo carrera = map (corra tiempo) carrera

usaPowerUp :: String -> PowerUp -> (CarreraAutos -> CarreraAutos)
usaPowerUp colorDelAuto powerUp carrera = flip powerUp carrera . buscarAuto colorDelAuto $ carrera

buscarAuto :: String -> [Auto] -> Auto
buscarAuto colorDelAuto = head . filter ((== colorDelAuto).color)

simularCarrera :: CarreraAutos -> [CarreraAutos -> CarreraAutos] -> [(Int, String)]
simularCarrera carrera eventos
  = ( tablaDePosiciones . foldl (flip ($)) carrera ) eventos

tablaDePosiciones :: CarreraAutos -> [(Int, String)]
tablaDePosiciones carrera = map (\auto -> (puesto auto carrera, color auto)) carrera

tablaDePosiciones' carrera = zip (map (flip puesto carrera) carrera) (map color carrera)


--5.
--a. Si sería posible con la funcion afectarALosQueCumplen, tomando como criterio que el color del auto sea igual al color deseado

{-b. No termina de evaluar, ya que necesitamos evaluar todos los casos (imposible) para saber a cuantos le esta ganando
El 1.b termina de evaluar si y solo si encuentra un auto que esté cerca del nuestro o encuentra alguno que haya recorrido mas distancia que el auto seleccionado. En caso contrario
estaría evaluando infinitamente.
-}