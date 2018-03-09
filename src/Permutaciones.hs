-- Permutaciones.hs
-- Cifrado por permutaciones.
-- Sevilla, 23 de Febrero de 2016
-- ---------------------------------------------------------------------
 
module Permutaciones where
 
import Generadores (mensaje)
import Test.QuickCheck 

-- (ordena xs) es la lista de los segundos elementos de xs ordenada
-- según los valores de los primeros elementos. Por ejemplo,
--    ordena [(2,4),(1,3),(3,1)]  == [3,4,1]
--    ordena [(2,1),(1,8),(3,5)]  == [8,5,1]
ordena :: Ord a => [(a,b)] -> [b]
ordena [] = []
ordena ((n,x):xs) = ordena anteriores ++ x : ordena posteriores
    where anteriores  = [(m,y) | (m,y) <- xs, m <= n]
          posteriores = [(k,z) | (k,z) <- xs, k > n]


-- (permuta ns xs) es la permutación de la lista xs en función de la
-- permutación ns. Las permutaciones vendran dadas por una lista ns de
-- forma que el primer numero de la permutación nos dice a qué posición
-- va el primer elemento de la lista xs. Por ejemplo,
--    permuta [2,1,3]   "abc"  == "bac"
--    permuta [4,3,2,1] "abcd" == "dcba"
permuta :: [Int] -> [a] -> [a]
permuta ns xs = ordena (zip ns xs)

-- (permutaInv xs) es la inversa de la permutación xs. Por ejemplo,
--    permutaInv [4,3,2,1] == [4,3,2,1]
--    permutaInv [4,1,2,3] == [2,3,4,1]
permutaInv :: [Int] -> [Int]
permutaInv ns = permuta ns [1..]

-- Ya estamos en disposición de cifrar y descifrar el mensaje usando el
-- criptosistema de permutaciones.

-- NOTA: si el mensaje no es divisible por la longitud de la permutación
-- dejaremos los últimos términos invariantes.

-- (permutacionF ks m) es el mensaje obtenido cifrando m mediante
-- permutación con las claves ks. Por ejemplo,
--    permutacionF [1,3,4,2] "ELALGEBRAESGENEROSAAMENUDODAMASDELOQUESELEPIDE"
--    ==  "ELLAGREBAGESERNEOASAMUENDAODMDASEQLOUEESLIEPDE"
permutacionF :: [Int] -> String -> String
permutacionF _  [] = []
permutacionF ks m 
    | length ks <= length m = 
        permuta ks (take (length ks) m) ++ 
        permutacionF ks (drop (length ks) m) 
    | otherwise = m

-- (permutacionF ks m) es el mensaje obtenido descifrando m mediante
-- permutación con las claves ks. Por ejemplo,
--    permutacionG [1,3,4,2]  "ELLAGREBAGESERNEOASAMUENDAODMDASEQLOUEESLIEPDE"
--    ==  "ENTODOLAMEDIDA"
permutacionG :: [Int] -> String -> String
permutacionG ks = permutacionF (permutaInv ks)

-- ---------------------------------------------------------------------
-- Vamos a comprobar con QuickCheck la corrección del criptosistema:

-- permutacion es un generador de permutaciones. Por ejemplo,
--    ghci> generate permutacion
--    [3,1,2]
--    ghci> generate permutacion
--    [1,3,5,6,4,2]
permutacion :: Gen [Int]
permutacion = do n <- choose (2,6)
                 shuffle [1..n]
           
-- mensajeYclavePerm es un generador de mensajes y claves. Por ejemplo,
--    λ> generate mensajeYclavePerm
--    ("SH",[1,2])
--    λ> generate mensajeYclavePerm
--    ("GTPPEYAFGXLRMX",[2,1])
--    λ> generate mensajeYclavePerm
--    ("HJRCYCNHSBFYTZBNQJCMBTQKZBXRQ",[5,3,4,6,1,2])

mensajeYclavePerm :: Gen (String,[Int])
mensajeYclavePerm = do m <- mensaje
                       k <- permutacion
                       return (m,k)

-- Propiedad: Al cifrar un mensaje con hillF y descifrarlo con
-- hillG se obtiene el mensaje original.
prop_CorreccionPerm :: Property
prop_CorreccionPerm = 
    forAll mensajeYclavePerm 
           (\(m,k)-> m == permutacionG k (permutacionF k m))

-- La comprobacion en consola es:
--   ghci> quickCheck prop_CorreccionPerm
--   +++ OK, passed 100 tests.
