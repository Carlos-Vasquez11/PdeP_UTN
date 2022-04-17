
--Creamos nuestros tipos de datos
type Libro = (String, String, Int)

elVisitante :: Libro
elVisitante = ("El Visitante","Stephen King",700)

shingeki1 :: Libro
shingeki1 = ("Shingeki no Kyojin 1","Hajime Isayama",40)

shingeki3 :: Libro
shingeki3 = ("Shingeki no Kyojin 3","Hajime Isayama",40)

shingeki127 :: Libro
shingeki127 = ("Shingeki no Kyojin 127","Hajime Isayama",40)

fundacion :: Libro
fundacion =  ("Fundacion","Isaac Asimov",230)

sandman5 :: Libro
sandman5 = ("Sandman","Neil Gaiman",35)

sandman10 :: Libro
sandman10 = ("Sandman","Neil Gaiman",35)

sandman12 :: Libro
sandman12 = ("Sandman","Neil Gaiman",35)

eragon :: Libro
eragon = ("Eragon","Christopher Paolini",544)

eldest :: Libro
eldest = ("Eldest","Christopher Paolini",704)

brisignr :: Libro
brisignr = ("Brisignr","Christopher Paolini",700)

legado :: Libro
legado = ("Legado","Christopher Paolini",811)

type Biblioteca = [Libro]
biblioteca :: Biblioteca
biblioteca = [elVisitante,shingeki1,shingeki3,shingeki127,fundacion,sandman5,sandman10,sandman12,eragon,eldest,brisignr,legado]

cantidadDePaginas :: (a,b,Int) -> Int
cantidadDePaginas (_,_,paginas) = paginas

totalDePaginas :: Biblioteca -> Int
totalDePaginas miBiblioteca = sum.(map cantidadDePaginas) $ miBiblioteca

promedioDePaginas :: Biblioteca -> Int
promedioDePaginas biblioteca = div (totalDePaginas biblioteca) (length biblioteca)


devolverAutor :: (a,String,b) -> String
devolverAutor (_,autor,_) = autor

lecturaObligatoria :: Libro -> String
lecturaObligatoria libro | devolverAutor libro == "Stephen King" || devolverAutor libro == "Christopher Paolini" || devolverAutor libro == "Isaac Asimov" = "¡Es Lectura Obligatoria!"
                         | otherwise = "¡No Es Lectura Obligatoria!"
