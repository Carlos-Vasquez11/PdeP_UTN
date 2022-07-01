data Persona = Persona{
    nombre :: String,
    edad   :: Int
} deriving (Show,Eq)

juan = Persona "Juan" 21

pedro = Persona{nombre = "Pedro", edad = 17}

newName =   (++) "New "

cambiarNombre :: Persona -> Persona
cambiarNombre unaPersona = unaPersona{nombre = newName.nombre $ unaPersona}

type Libro = (String, String)

miLibro :: Libro
miLibro = ("Stephen King", "El instituto")

miFuncion = map (enumFromTo 1) [1,3,5]