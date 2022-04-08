--Funciones Dadas
--take :: Int -> String -> String

--drop :: Int -> String -> String

--head :: String -> Char

--elem :: Char -> String -> Bool

--reverse :: String -> String



--Desarrollo
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}

--Nota: podemos quitar la etiqueta "dia" y la función seguiría funcionando, pero considero que es poco expresivo.
entregaSencilla :: String -> Bool
entregaSencilla dia = even.length $ dia

productoXL :: String -> String
productoXL nombreProducto = nombreProducto ++ " XL"


descodiciarProducto :: String -> String
descodiciarProducto nombreProducto = take 10 nombreProducto 

versionBarata :: String -> String
versionBarata nombreProducto = reverse.descodiciarProducto $ nombreProducto


productoDeLujo :: String -> Bool
productoDeLujo nombreProducto = elem 'x' nombreProducto ||  elem 'z' nombreProducto

productoCodiciado :: String -> Bool
productoCodiciado nombreProducto = length nombreProducto > 10

productoCorriente :: String -> Bool
productoCorriente nombreProducto = head nombreProducto == 'a' || head nombreProducto == 'e' || head nombreProducto == 'i' || head nombreProducto == 'o' || head nombreProducto == 'u'

productoDeElite :: String -> Bool
productoDeElite nombreProducto = productoDeLujo nombreProducto && productoCodiciado nombreProducto && not (productoCorriente nombreProducto)


aplicarDescuento :: Fractional a => a -> a -> a
aplicarDescuento precio descuento = precio - precio * descuento / 100

aplicarCostoDeEnvio :: Num a => a -> a -> a
aplicarCostoDeEnvio precio costoEnvio = precio + costoEnvio


--Para hacer la composición debemos descomponer el código en funciones mas pequeñas
precioTotal :: Fractional a => a -> a -> a -> a -> a
precioTotal precioUnitario cantidad descuento costoEnvio = aplicarCostoDeEnvio ((aplicarDescuento precioUnitario descuento) * cantidad) costoEnvio
--Nota:" (aplicarDescuento precioUnitario descuento) * cantidad ".No necesita parentesis, pero considero que esto ayuda a leer un poco mejor el código

--Preguntar si podemos utilizar operadores lógicos && ||