--Funciones Dadas
--Nota: abajo de tipar las funciones por mi mismo puse la verdadera declaración de cada función.
--take :: Int -> String -> String
--take :: forall a. Int -> [a] -> [a]

--drop :: Int -> String -> String
--drop :: forall a. Int -> [a] -> [a]

--head :: String -> Char
--head :: forall a. [a] -> a

--elem :: Char -> String -> Bool
--elem :: forall (t :: * -> *) a. (Foldable t, Eq a) => a -> t a -> Bool

--reverse :: String -> String
--reverse :: forall a. [a] -> [a]

--Desarrollo
--Aquí estoy muteando las sugerencias para que no me moleste VSCode
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}

data Producto = UnProducto {
    nombreDelProducto :: String,
    precioDelProducto :: Int    
} deriving (Eq, Show)

remera :: Producto
remera = UnProducto "Remera" 2000

entregaSencilla :: String -> Bool
entregaSencilla = even.length

productoXL :: Producto -> String
productoXL (UnProducto nombreProducto _) = nombreProducto ++ " XL"


descodiciarProducto :: Producto -> String
descodiciarProducto (UnProducto nombreProducto _) = take 10 nombreProducto 

versionBarata :: Producto -> String
versionBarata = reverse.descodiciarProducto


productoDeLujo :: String -> Bool
productoDeLujo nombreProducto =  'x' `elem` nombreProducto || 'z' `elem` nombreProducto

productoCodiciado :: String -> Bool
productoCodiciado  = (>10).length

esVocal :: Char -> Bool
esVocal caracter = elem caracter ['a','e','i','o','u','A','E','I','O','U']

productoCorriente :: String -> Bool
productoCorriente = esVocal.head


productoDeElite :: Producto -> Bool
productoDeElite (UnProducto nombreProducto _) = productoDeLujo nombreProducto && productoCodiciado nombreProducto && not (productoCorriente nombreProducto)


aplicarDescuento :: Fractional a => a -> a -> a
aplicarDescuento precio descuento = precio - precio * descuento / 100

aplicarCostoDeEnvio :: Num a => a -> a -> a
aplicarCostoDeEnvio precio costoEnvio = precio + costoEnvio


multiplicarCantidad :: Num a => a -> a -> a
multiplicarCantidad valor cantidad = valor * cantidad

precioTotal :: Fractional a => (String,a) -> a -> a -> a -> a
precioTotal (_, precioUnitario) cantidad descuento costoEnvio = (aplicarCostoDeEnvio costoEnvio).(multiplicarCantidad cantidad).(aplicarDescuento precioUnitario) $ descuento
