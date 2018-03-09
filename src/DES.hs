-- DES.hs
-- Cifrado DES
-- Sevilla, 7 de Marzo de 2016
-- ---------------------------------------------------------------------
 
module DES where

import Asociacion2 (codifica, descodifica)
import Asociacion3 (int2bin4, bin2int4, xor)
import Data.Matrix
import Test.QuickCheck

------------------------------------------------------------------------------
-- Antes de nada definimos distintos ejemplos de mensajes, claves,
-- y mensajes ya encriptados, obtenidos de distintos artículos:

-- Ejemplos de mensajes:
ejemploM :: String
ejemploM = "ABCEDFGH"

ejM :: [Int]
ejM = [1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,
       0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,0,0,0,0,1,0,1,0,1,0,1,0]

ejMensaje :: [Int]
ejMensaje = [0,0,0,0, 0,0,0,1, 0,0,1,0, 0,0,1,1, 0,1,0,0, 0,1,0,1, 0,1,1,0,
             0,1,1,1, 1,0,0,0, 1,0,0,1, 1,0,1,0, 1,0,1,1, 1,1,0,0, 1,1,0,1,
             1,1,1,0, 1,1,1,1]

mensaje_ej2 :: [Int]
mensaje_ej2 = [0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,1,1,
               1,0,0,0,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1]

-- Ejemplos de claves:
clave_ej2 :: [Int]
clave_ej2 = [0,0,0,1,0,0,1,1,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,1,0,1,1,1,1,0,0,1,1,
             0,0,1,1,0,1,1,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,1,1,1,1,1,0,0,0,1]

ejemploK :: [Int]
ejemploK = [0,0,0,1,0,0,1,1,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,1,0,1,1,1,1,0,0,1,1,0,
            0,1,1,0,1,1,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,1,1,1,1,1,0,0,0,1]
-- Ejemplo de subclave
ejemplok :: [Int]
ejemplok = [0,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,1,1,1,0,1,1,1,1,1,1,1,1,1,1,0,
            0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,1,0]

ejemploL :: [Int]
ejemploL = [1,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,1,1,0,0,0,0,1,0,1,0,1,0,0,0,1,0]

ejemploR :: [Int]
ejemploR = [0,0,1,0,0,0,1,0,0,1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,0,0,0,1,0,0,1,0]
-- mensaje_ej2 cifrado con la clave_ej2:
mensaje_cifrado_ej2 :: [Int]
mensaje_cifrado_ej2 =
    [1,0,0,0,0,1,0,1,1,1,1,0,1,0,0,0,0,0,0,1,0,0,1,1,0,1,0,1,
     0,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,0,1,0,1,0,1,1,0,1,0,0,0,0,0,0,0,1,0,1]

-- ejMensaje cifrado con ejemploK:
ejemploencriptado :: [Int]
ejemploencriptado =
    [1,0,0,0,0,1,0,1, 1,1,1,0,1,0,0,0, 0,0,0,1,0,0,1,1, 0,1,0,1,0,1,0,0,
     0,0,0,0,1,1,1,1, 0,0,0,0,1,0,1,0, 1,0,1,1,0,1,0,0, 0,0,0,0,0,1,0,1]

-- ---------------------------------------------------------------------

-- En este módulo las permutaciones las definiremos de forma distinta:

-- (expansion ns xs) es la permutación de la lista xs en función de la
-- permutación ns. Las permutaciones vendrán dadas por una lista ns de
-- forma que el primer elemento de xs aparecerá en todas las posiciones
-- de ns donde aparezca el 1, y así sucesivamente. Cabe destacar la 
-- posibilidad de que la longitud de la lista de salida sea mayor que 
-- la de xs, ya que es ns quien define la longitud de salida. Por ejemplo,
--    expansion [2,1,3]   "abc"        == "bac"
--    expansion [4,1,3,2] "abcd"       == "dacb"
--    expansion [1,1,4,1,3,2,1] "abcd" == "aadacba"
expansionR :: [Int] -> [a] -> [a]
expansionR [] _      = []
expansionR (e:xs) ys = (ys!!(e-1)) : expansionR xs ys

-- Una definicion alternativa usando plegados:
expansion :: [Int] -> [a] -> [a]
expansion xs ys = foldr (\x y-> (ys!!(x-1)):y) [] xs

-- Implementamos un generador de expansiones que genera una de longitud n
-- (ya dada) y crea una nueva repitiendola.
--     ghci> generate (expansionEj "ab")
--     [1,2,1,2]
--     ghci> generate (expansionEj "ab")
--     [2,1,2,1]
expansionGen :: [a] -> Gen [Int]
expansionGen xs = do k <- shuffle [1..(length xs)]
                     return (k++k)

-- Comprobemos que las definiciones son equivalentes:
prop_Expansion :: String -> Property
prop_Expansion xs = 
    forAll (expansionGen xs)
           (\q -> expansionR q xs == expansion q xs)

-- La comprobacion en consola es:
--     ghci> quickCheck prop_Expansion
--     +++ OK, passed 100 tests.

-- Veamos cual es mas eficiente:
--     ghci> :set +s
--     ghci>  length (expansionR [1..(10^7)] (replicate (10^7) ('a')))
--     10000000
--     (4.42 secs, 2563634096 bytes)
--     ghci>  length (expansion [1..(10^7)] (replicate (10^7) ('a')))
--     10000000
--     (1.66 secs, 1843532320 bytes)

-- Asi que nos quedamos con la segunda definicion.
------------------------------------------------------------------------------

-- Paso 1: Crear 16 sub-claves de 48 bits cada una
--

-- La permutación que aplicaremos en nuestra clave es:
pc_1 :: [Int]
pc_1 = [57,49,41,33,25,17,9,1,58,50,42,34,26,18,10,2,59,51,43,35,27,19,11,3,60,
        52,44,36,63,55,47,39,31,23,15,7,62,54,46,38,30,22,14,6,61,53,45,37,29,
        21,13,5,28,20,12,4]

-- A continuación, nos hará falta dividir la clave permutada en dos mitades.

-- (mitades xs) es el par formado por las dos mitades de xs. Por ejemplo,
--    mitades [1..4]  ==  ([1,2],[3,4])
--    mitades [1..5]  ==  ([1,2],[3,4,5])
mitades :: [a] -> ([a],[a])
mitades xs = splitAt (length xs `div` 2) xs

-- (desplazaIzq n xs) es la lista obtenida desplazando xs n posiciones
-- de derecha a izquierda. Por ejemplo, 
--    desplazaIzq 1 [1,2,3]   == [2,3,1]
--    desplazaIzq 2 [1,2,3,4] == [3,4,1,2]
desplazaIzq :: Int -> [Int] -> [Int]
desplazaIzq n xs = drop n xs ++ take n xs

-- Haremos 16 veces este desplazamiento siguiendo este patrón:
desplazamientos :: [Int]
desplazamientos = [1,2,4,6,8,10,12,14,15,17,19,21,23,25,27,28]

-- Por último, a cada clave desplazada anteriormente (son 16), le aplicaremos
-- la siguiente permutación. Obteniendo así 16 claves de 48 bits.
pc_2 :: [Int]
pc_2 = [14,17,11,24,1,5,3,28,15,6,21,10,23,19,12,4,26,8,16,7,27,20,13,2,41,52,
        31,37,47,55,30,40,51,45,33,48,44,49,39,56,34,53,46,42,50,36,29,32]

-- Un ejemplo de 16 claves a partir de una es:

        
-- Para aglutinar todo esto definimos una función que llamaremos
-- "crea16Claves", que dada la clave de 64 bits nos dará 16 claves
-- de 48 bits cada una. Por ejemplo,
--    λ> crea16Claves ejemploK
---   [[1,1,1,1,1,1,0,1,1,1,1,0,0,1,0,1,0,0,0,0,1,1,0,0,0,1,1,0,0,0,1,
--      1,0,0,1,1,0,1,0,1,1,1,1,1,1,0,1,1],
--     [1,1,0,1,1,0,0,0,0,1,1,1,1,1,0,1,1,0,1,0,1,1,0,1,0,0,1,0,1,0,0,
--      0,1,1,1,1,1,0,1,1,0,0,1,1,1,0,1,0],
--     [0,1,1,0,1,0,0,0,1,1,1,1,1,1,0,1,1,1,0,1,1,0,1,0,0,1,1,0,1,1,0,
--      1,0,0,1,0,1,0,1,0,0,0,0,1,0,1,1,0],
--     [1,0,1,0,0,1,1,0,1,0,0,1,1,0,1,1,0,1,1,1,1,0,1,0,0,1,1,1,1,1,0,
--      1,1,0,1,0,0,0,0,1,0,0,0,1,1,0,0,1],
--     [1,1,0,0,0,1,1,1,0,1,0,1,0,1,1,1,0,0,1,0,0,1,1,1,1,1,1,1,0,0,1,
--      1,1,0,1,0,1,0,0,1,0,1,0,1,1,0,0,0],
--     [0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,0,0,1,0,0,0,1,1,1,1,1,0,1,1,0,1,
--      1,0,0,1,1,1,0,0,1,1,0,1,0,1,0,0,0],
--     [1,1,1,1,1,0,1,0,0,0,1,1,1,0,1,0,1,1,0,1,1,1,1,0,1,0,1,1,1,0,1,
--      0,0,0,0,1,0,1,1,1,0,0,1,1,1,1,0,0],
--     [1,1,0,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,0,0,1,1,1,0,1,1,
--      0,0,1,1,1,0,0,1,0,0,0,1,1,0,1,0,1],
--     [0,1,0,1,0,1,1,1,1,1,0,1,1,0,1,1,1,0,1,1,1,1,1,0,1,1,0,1,0,1,0,
--      0,1,1,0,1,0,0,1,0,0,0,1,0,1,1,1,1],
--     [0,1,0,1,0,1,0,1,1,0,1,0,1,1,1,1,1,1,1,0,0,1,1,1,0,0,1,1,0,1,1,
--      1,1,0,0,0,1,0,1,1,0,1,1,0,0,1,1,1],
--     [0,0,1,1,1,1,1,0,1,0,1,0,0,1,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,1,
--      1,1,0,1,1,1,0,1,0,1,1,0,0,1,0,0,1],
--     [1,1,1,0,1,1,1,1,1,0,1,1,0,0,0,0,1,0,1,1,1,1,0,1,0,0,0,1,1,1,1,
--      0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,0,0],
--     [1,1,0,1,1,1,0,1,1,1,1,1,0,1,0,1,0,1,1,1,1,0,1,1,0,1,0,0,1,0,1,
--      0,1,1,0,1,1,1,0,0,1,0,0,1,0,1,0,1],
--     [0,0,1,1,1,1,1,0,0,1,0,0,1,1,1,1,1,1,1,1,1,0,1,1,0,1,0,1,1,0,0,
--      1,0,1,0,1,1,1,0,1,0,1,0,1,0,1,1,1],
--     [1,0,1,0,1,0,1,1,1,0,0,0,1,1,1,1,1,0,1,0,1,1,0,1,1,1,1,1,1,0,1,
--      1,0,1,0,0,0,0,0,1,1,1,0,1,1,1,1,1],
--     [1,0,1,1,1,1,0,1,0,1,0,0,1,0,1,0,0,0,1,1,1,1,0,1,1,1,1,0,1,1,1,
--      1,1,1,1,0,1,1,0,1,0,0,1,0,1,0,1,0]]
crea16Claves :: [Int] -> [[Int]]
crea16Claves ks = [expansion pc_2 k | k <- ks'']
                  where (xs,ys) = mitades ks'
                        ks'     = expansion pc_1 ks
                        ks''    = [desplazaIzq n xs ++ desplazaIzq n ys
                                   | n <- desplazamientos]
   
-- ---------------------------------------------------------------------
-- § Paso 2: cifrar el mensaje                                        --
-- ---------------------------------------------------------------------

-- En primer lugar, se realiza una permutación inicial "ip" sobre el mensaje

ip :: [Int]
ip = [58,50,42,34,26,18,10,2,60,52,44,36,28,20,12,4,62,54,46,38,30,22,14,6,
      64,56,48,40,32,24,16,8,57,49,41,33,25,17,9,1,59,51,43,35,27,19,11,3,
      61,53,45,37,29,21,13,5,63,55,47,39,31,23,15,7]
     
-- Ahora programaremos las rondas de Feistel, necesarias en el
-- criptosistema DES. Una ronda de Feistel consta de 3 partes:
-- 1) Dividimos el mensaje en dos m = (l,r) usando la función "mitades".
-- 2) Calculamos F(k,r) de una cierta función F dada
-- 3) Calculamos M = [r | xor l F(k,r)]
--
-- Para calcular la función F nos hará falta realizar una expansión de cada
-- mitad para lo cual nos sirve la función "expansion" definida en el paso 1.
-- La expansión viene definida por:

ex :: [Int]
ex = [32,1,2,3,4,5,4,5,6,7,8,9,8,9,10,11,12,13,12,13,14,15,16,17,16,17,18,19,
      20,21,20,21,22,23,24,25,24,25,26,27,28,29,28,29,30,31,32,1]

-- La siguiente tarea a realizar será sumar mediante la función "xor" la
-- expansión de r y la clave que nos corresponda (una de las 16 del paso 1).
-- Lo obtenido lo dividiremos en 8 listas de 6 bits haciendo pasar a cada
-- lista por una S-caja. Las S-cajas son matrices de las que extraeremos un
-- número de la siguiente forma:
-- + el primer y el último bit del bloque, en base 2, representan la fila
-- + los 4 bits restantes (en el centro) representan la columna
--
-- Las S-cajas son:

s1, s2, s3, s4, s5, s6, s7, s8 :: Matrix Int
s1 = fromLists [[14,4,13,1,2,15,11,8,3,10,6,12,5,9,0,7],
                [0,15,7,4,14,2,13,1,10,6,12,11,9,5,3,8],
                [4,1,14,8,13,6,2,11,15,12,9,7,3,10,5,0],
                [15,12,8,2,4,9,1,7,5,11,3,14,10,0,6,13]]

s2 = fromLists [[15,1,8,14,6,11,3,4,9,7,2,13,12,0,5,10],
                [3,13,4,7,15,2,8,14,12,0,1,10,6,9,11,5],
                [0,14,7,11,10,4,13,1,5,8,12,6,9,3,2,15],
                [13,8,10,1,3,15,4,2,11,6,7,12,0,5,14,9]]

s3 = fromLists [[10,0,9,14,6,3,15,5,1,13,12,7,11,4,2,8],
                [13,7,0,9,3,4,6,10,2,8,5,14,12,11,15,1],
                [13,6,4,9,8,15,3,0,11,1,2,12,5,10,14,7],
                [1,10,13,0,6,9,8,7,4,15,14,3,11,5,2,12]]

s4 = fromLists [[7,13,14,3,0,6,9,10,1,2,8,5,11,12,4,15],
                [13,8,11,5,6,15,0,3,4,7,2,12,1,10,14,9],
                [10,6,9,0,12,11,7,13,15,1,3,14,5,2,8,4],
                [3,15,0,6,10,1,13,8,9,4,5,11,12,7,2,14]]

s5 = fromLists [[2,12,4,1,7,10,11,6,8,5,3,15,13,0,14,9],
                [14,11,2,12,4,7,13,1,5,0,15,10,3,9,8,6],
                [4,2,1,11,10,13,7,8,15,9,12,5,6,3,0,14],
                [11,8,12,7,1,14,2,13,6,15,0,9,10,4,5,3]]

s6 = fromLists [[12,1,10,15,9,2,6,8,0,13,3,4,14,7,5,11],
                [10,15,4,2,7,12,9,5,6,1,13,14,0,11,3,8],
                [9,14,15,5,2,8,12,3,7,0,4,10,1,13,11,6],
                [4,3,2,12,9,5,15,10,11,14,1,7,6,0,8,13]]

s7 = fromLists [[4,11,2,14,15,0,8,13,3,12,9,7,5,10,6,1],
                [13,0,11,7,4,9,1,10,14,3,5,12,2,15,8,6],
                [1,4,11,13,12,3,7,14,10,15,6,8,0,5,9,2],
                [6,11,13,8,1,4,10,7,9,5,0,15,14,2,3,12]]

s8 = fromLists [[13,2,8,4,6,15,11,1,10,9,3,14,5,0,12,7],
                [1,15,13,8,10,3,7,4,12,5,6,11,0,14,9,2],
                [7,11,4,1,9,12,14,2,0,6,10,13,15,3,5,8],
                [2,1,14,7,4,10,8,13,15,12,9,0,3,5,6,11]]

-- (divideEn8 xs) es la lista obtenida dividiendo xs en grupos de 6. Por
-- ejemplo, 
--    λ> divideEn8 [1..20]
--    [[1,2,3,4,5,6],[7,8,9,10,11,12],[13,14,15,16,17,18],[19,20]]
divideEn8 :: [Int] -> [[Int]]
divideEn8 [] = []
divideEn8 xs = take 6 xs : divideEn8 (drop 6 xs)

-- La función que recibe un par (bloque,Scaja); y al bloque de 6 le aplica
-- la S-caja es:
sCaja :: ([Int],Matrix Int) -> [Int]
sCaja ([a,b,c,d,e,f],s) = int2bin4 (s!(i,j))
    where i = 1 + bin2int4 [a,f]
          j = 1 + bin2int4 [b,c,d,e]
sCaja _ = []
-- El último paso para calcular la F es hacer una permutación definida por:
p :: [Int]
p = [16,7,20,21,29,12,28,17,1,15,23,26,5,18,31,10,2,8,24,14,32,27,3,9,19,13,
     30,6,22,11,4,25]

-- Quedando la función F de la ronda de Feistel:
funcionF :: [Int] -> [Int] -> [Int]
funcionF k r =
    expansion p (concat [sCaja a
                          | a <-zip (divideEn8 (xor (expansion ex r) k))
                                    [s1,s2,s3,s4,s5,s6,s7,s8]])

-- Finalmente una ronda de Feistel es:
rondaFeistel :: [Int] -> [Int] -> [Int]
rondaFeistel k m = r ++ xor l (funcionF k r)
      where (l,r) = mitades m

-- Y las 16 rondas de Feistel:
rondasFeistel :: [Int] -> [Int] -> [Int]
rondasFeistel ks = aux (crea16Claves ks)
    where aux [] m      = snd (mitades m) ++ fst (mitades m)
          aux (k:ks') m = aux ks' (rondaFeistel k m)

-- Después de las rondas de Feistel deshacemos la permutación IP del principio:

invIP :: [Int]
invIP =
    [40,8,48,16,56,24,64,32,39,7,47,15,55,23,63,31,38,6,46,14,54,22,62,30,
     37,5,45,13,53,21,61,29,36,4,44,12,52,20,60,28,35,3,43,11,51,19,59,27,
     34,2,42,10,50,18,58,26,33,1,41,9,49,17,57,25]

encriptaDES' :: [Int] -> [Int] -> [Int]
encriptaDES' ks m
    | length m == 64 =
        expansion invIP (rondasFeistel ks (expansion ip m))
    | otherwise = m

encriptaDES :: String -> String -> String
encriptaDES ks m =
    descodifica (aux (codifica ks) (codifica m) [])
    where aux _ [] xs = xs 
          aux k ms xs =
              aux k (drop 64 ms) (xs ++ encriptaDES' k (take 64 ms))

rondasFeistelInv :: [Int] -> [Int] -> [Int]
rondasFeistelInv ks = aux (reverse(crea16Claves ks))
    where aux [] m      = snd (mitades m) ++ fst (mitades m)
          aux (k:ks') m = aux ks' (rondaFeistel k m) 

desencriptaDES' :: [Int] -> [Int] -> [Int]
desencriptaDES' ks m
    |length m == 64 =
        expansion invIP (rondasFeistelInv ks (expansion ip m))
    | otherwise = m

desencriptaDES :: String -> String -> String
desencriptaDES ks m =
    descodifica (aux (codifica ks) (codifica m) [])
    where aux _ [] xs = xs 
          aux k ms xs =
              aux k (drop 64 ms) (xs ++ desencriptaDES' k (take 64 ms))

-- -------------------------------------------------------------------------

-- Vamos a comprobar con QuickCheck la corrección del criptosistema:

claveYmensajeDES :: Gen (String,String)
claveYmensajeDES = do n <- choose (1,6)
                      k <- vector 8
                      m <- vector (8*n)
                      return (k,m)

-- Propiedad: Al cifrar un mensaje con encriptaDES y descifrarlo con
-- desencriptaDES se obtiene el mensaje original.
prop_CorreccionDES :: Property
prop_CorreccionDES = 
    forAll claveYmensajeDES 
           (\(k,m) -> m == desencriptaDES k (encriptaDES k m))

-- La comprobación es
--    λ> quickCheck prop_CorreccionDES
--    +++ OK, passed 100 tests.
