-- Afin.hs
-- Cifrado afín.
-- Sevilla, 25 de Noviembre de 2015
-- ---------------------------------------------------------------------

module Afin where

import Asociacion1 (int2str, str2int, invMod, enAbecedario) 
import Criptoanalisis (ordena2, frecuencias)
import Generadores (mensaje)
import Test.QuickCheck

-- (afinF (k1,k2) m) es el mensaje obtenido encriptando m con el
-- método afín usando las clves k1 y k2. Por ejemplo,
--    afinF (3,3) "ENTODOLAMEDIDA"  ==  "PQITMTKDNPMBMD"
afinF :: (Int,Int) -> String -> String
afinF (k1,k2) m = int2str [((n*k1)+k2) `mod` 26 | n <- str2int m]

-- (afinG (k1,k2) m) es el mensaje obtenido desencriptando m con el
-- método afín usando las clves k1 y k2. Por ejemplo,
--    afinG (3,3) "PQITMTKDNPMBMD" == "ENTODOLAMEDIDA"

-- Comentario: He cambiado la definición de afinG
--    afinG :: (Int,Int) -> String -> String
--    afinG (k1,k2) m' = 
--        int2str [((n-k2) * (fromIntegral (invMod (toInteger k1) 26)) `mod` 26)
--                    | n <- str2int m']

afinG :: (Int,Int) -> String -> String
afinG (k1,k2) m' = 
    int2str [((n-k2) * invMod k1 26) `mod` 26 | n <- str2int m']

-- Vamos a comprobar con QuickCheck la corrección del criptosistema:

-- (primocon26 n) se verifica si n es primo con 26. Por ejemplo,
--    primocon26 2 == False
--    primocon26 17 == True
primocon26 :: Int -> Bool
primocon26 n = gcd n 26 == 1

-- (mensajeYclaveOK m (k1,k2)) verifica si el mensaje m está escrito en
-- mayúsculas y si la clave es válida para el cifrado afín. Por ejemplo,c
--    mensajeYclaveOK ("ENTODOLAMEDIDA",(5,9))  ==  True
--    mensajeYclaveOK ("ENTODOLAMEDIDA",(5,9))  ==  False
--    mensajeYclaveOK ("ENTODOLAMEDIDA",(8,9))  ==  False
--    mensajeYclaveOK ("ENTODOLAMEDIDA",(5,8))  ==  True
mensajeYclaveOK :: (String,(Int,Int)) -> Bool
mensajeYclaveOK (m,(k1,_)) = enAbecedario m && primocon26 k1

-- mensajeYclave es un generador de mensajes y claves correctas. Por ejemplo,
--    generate mensajeYclave  ==  ("VFNJEIOUPWEALEXMSZSWXASIR",(7,11))
--    generate mensajeYclave  ==  ("KELQJECUJMYUGM",(21,2))
--    generate mensajeYclave  ==  ("AEMDWHQJ",(17,16))
mensajeYclaveA :: Gen (String,(Int,Int))
mensajeYclaveA = do m <- mensaje
                    k1 <- suchThat (choose (0,26)) primocon26
                    k2 <- choose (0,26)
                    return (m,(k1,k2))

-- Propiedad: Al cifrar un mensaje con afinF y descrifrarlo con
-- afinG se obtiene el mensaje original.
prop_CorreccionAfin :: Property
prop_CorreccionAfin = 
    forAll mensajeYclaveA (\(m,k)-> m == afinG k (afinF k m))

-- La comprobación es
--    ghci> quickCheck prop_CorreccionAfin
--    +++ OK, passed 100 tests.


-- ---------------------------------------------------------------------
-- Criptoanálisis
-- ---------------------------------------------------------------------

-- (letrasFrecuentes cs) es la lista de los dos caracteres más
-- frecuentes de cs. Por ejemplo,
--    letrasFrecuentes "TODOPARANADA"  ==  "AO"
letrasFrecuentes :: String -> String
letrasFrecuentes m = take 2 (ordena2 (frecuencias m))

-- (despejak1 x y) es la solución de k1*(x-y) = 4, módulo 26. Por ejemplo,
--    despejak1 5 9  ==  25

-- Comentario: He cambiado la siguiente definición
--    despejak1 :: Int -> Int -> Int
--    despejak1 x y = 
--        fromIntegral (invMod (toInteger(head [k1 | k1 <- [1..25], 
--                           k1*(x-y) `mod` 26 == 4, 
--                           gcd k1 26 == 1])) 26
despejak1 :: Int -> Int -> Int
despejak1 x y = 
    invMod (head [k1 | k1 <- [1..25], 
                       k1 * (x - y) `mod` 26 == 4, 
                       gcd k1 26 == 1])
           26

-- (posibleClave m) es la clave usada para encriptar el mensaje m. Por ejemplo,
--    afinF (5,7) "LOBUENOSIBREVEDOSVECESBUENO"   ==  "KZMDBUZTVMOBIBWZTIBRBTMDBUZ"
--    posibleClave "KZMDBUZTVMOBIBWZTIBRBTMDBUZ"  ==  (7,25)o
posibleClave :: String -> (Int,Int)
posibleClave m = (despejak1 x y,y)
    where [x,y] = str2int (letrasFrecuentes m)

-- Comentario: Cambiar ejemplo para que adivine la clave.

-- (desencriptaAfin m) es el mensaje desencriptando m. Por ekemplo, 
--    afinF (5,7) "LAGEOMETRIAESELARTEDEPENSARBIENYDIBUJARMAL"   ==
--  "FCGWUKWTJQCWOWFCJTWRWZWPOCJHQWPSRQHYVCJKCF"
--    desencriptaAfin "FCGWUKWTJQCWOWFCJTWRWZWPOCJHQWPSRQHYVCJKCF"  ==
--  "LAGEOMETRIAESELARTEDEPENSARBIENYDIBUJARMAL" 
desencriptaAfin :: String -> String
desencriptaAfin m = afinG (posibleClave m) m

