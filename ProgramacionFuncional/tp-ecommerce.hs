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

--Nota: podemos quitar la etiqueta "dia" y la función seguiría funcionando, pero considero que es poco expresivo.
entregaSencilla :: String -> Bool
entregaSencilla dia = even.length $ dia

productoXL :: String -> String
productoXL nombreProducto = nombreProducto ++ " XL"


descodiciarProducto :: String -> String
descodiciarProducto nombreProducto = take 10 nombreProducto 

versionBarata :: String -> String
versionBarata = reverse.descodiciarProducto


productoDeLujo :: String -> Bool
productoDeLujo nombreProducto =  'x' `elem` nombreProducto || 'z' `elem` nombreProducto

productoCodiciado :: String -> Bool
productoCodiciado nombreProducto = length nombreProducto > 10

esVocal :: Char -> Bool
esVocal caracter = elem caracter ['a','e','i','o','u','A','E','I','O','U']

productoCorriente :: String -> Bool
productoCorriente nombreProducto = esVocal.head $ nombreProducto


productoDeElite :: String -> Bool
productoDeElite nombreProducto = productoDeLujo nombreProducto && productoCodiciado nombreProducto && not (productoCorriente nombreProducto)


aplicarDescuento :: Fractional a => a -> a -> a
aplicarDescuento precio descuento = precio - precio * descuento / 100

aplicarCostoDeEnvio :: Num a => a -> a -> a
aplicarCostoDeEnvio precio costoEnvio = precio + costoEnvio


multiplicarCantidad :: Num a => a -> a -> a
multiplicarCantidad valor cantidad = valor * cantidad

precioTotal :: Fractional a => (String,a) -> a -> a -> a -> a
precioTotal (nombre, precioUnitario) cantidad descuento costoEnvio = (aplicarCostoDeEnvio costoEnvio).(multiplicarCantidad cantidad).(aplicarDescuento precioUnitario) $ descuento
