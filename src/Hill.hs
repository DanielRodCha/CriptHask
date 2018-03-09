-- Hill.hs
-- Cifrado de Hill.
-- Sevilla, 22 de Febrero de 2016
-- ---------------------------------------------------------------------
 
module Hill where
 
import Asociacion1 (str2int, int2str, invMod)
import Data.Matrix 
import Generadores (mensaje)
import Test.QuickCheck

-- (divModular a b n) es el cociente de a entre b módulo n. Por ejemplo,
--    divModular 2 3 5  ==  4
divModular :: Int -> Int -> Int -> Int
divModular a b n = head [m | m <- [0..n-1], m*b `mod` n == a]

-- (invertible a) se verifica si la matriz a es invertible módulo
-- 26. Por ejemplo, 
--    invertible (fromLists [[2,1],[3,4]])  ==  True
--    invertible (fromLists [[2,1],[8,4]])  ==  False
invertible :: Matrix Int -> Bool
invertible a = 1 == gcd x 26
    where x = abs (detLaplace a)

-- (matrizAdj a) es la matriz adjunta de a. Por ejemplo,
--    ghci> matrizAdj (fromLists [[2,3],[1,1]])
--    (  1 -1 )
--    ( -3  2 )
matrizAdj :: Matrix Int -> Matrix Int
matrizAdj a = 
    matrix (nrows a) (ncols a) $ 
    \(i,j)-> (((-1)^(i+j)) * detLaplace (minorMatrix i j a))

-- (matrizAdjTrasp a) es la traspuesta de la adjunta de a. Por ejemplo, 
--    ghci> matrizAdjTrasp (fromLists [[2,3],[1,1]])
--    (  1 -3 )
--    ( -1  2 )
matrizAdjTrasp :: Matrix Int -> Matrix Int
matrizAdjTrasp = transpose . matrizAdj 

-- (matrizInv a) es la inversa de la matriz a. Por ejemplo,
--    ghci> matrizInv (fromLists [[2,3],[1,1]])
--    ( 25  3 )
--    (  1 24 )
--    ghci> matrizInv (fromLists [[2,3],[1,7]])
--    (  3 21 )
--    (  7 12 )

-- Comentario: Modificada la definición
--    matrizInv :: Matrix Int -> Matrix Int
--    matrizInv a | invertible a = matrizInvAux (matrizAdjTrasp a) (nrows a)
--                | otherwise     = error "Matriz no valida"
--        where matrizInvAux b 0 = b
--              matrizInvAux b n = 
--                  matrizInvAux (mapRow (\_ x -> ((x * t) `mod` 26)) n b) (n-1)
--              t = fromIntegral (invMod (toInteger ((detLaplace a) `mod`26)) 26)

matrizInv :: Matrix Int -> Matrix Int
matrizInv a
    | invertible a = matrizInvAux (matrizAdjTrasp a) (nrows a)
    | otherwise    = error "Matriz no valida"
    where
      matrizInvAux b 0 = b
      matrizInvAux b n = 
          matrizInvAux (mapRow (\_ x -> ((x * t) `mod` 26)) n b) (n-1)
      t = invMod (detLaplace a `mod`26) 26
              
-- Ya estamos en disposición de cifrar y descifrar con el código de Hill

-- (hillF ks m) es el mensaje m cifrado por el método de Hill con la
-- matriz ks. Por ejemplo,
--    ghci> hillF (fromLists [[2,1],[3,4]]) "LASMATEMATICASSONLAMUSICADELARAZON"
--    "WHWYTYUITYSGSUYGLFMWGCSGDMTERQZWPQ"
--    ghci> hillF (fromLists [[2,1],[8,4]]) "LASMATEMATICASSONLAMUSICADELARAZON"
--    "*** Exception: Matriz no valida
hillF :: Matrix Int -> String -> String
hillF xs n | invertible xs = aux xs n 
           | otherwise     = error "Matriz no valida"
    where aux _ [] = []
          aux ks m | nrows ks <= length m = 
                       int2str 
                        (toList 
                         (mapCol 
                          (\_ x -> x `mod` 26) 
                          1
                          (multStd ks (transpose (fromLists
                                   [str2int (take (nrows ks) m)])) 
                                   ))) ++ 
                       aux ks (drop (nrows ks) m)
                   | otherwise = m

-- (hillG ks m) es el mensaje m descifrado por el método de Hill con la
-- matriz ks. Por ejemplo,
--    ghci> hillG (fromLists [[2,1],[3,4]]) "WHWYTYUITYSGSUYGLFMWGCSGDMTERQZWPQ"
--    "LASMATEMATICASSONLAMUSICADELARAZON"
hillG :: Matrix Int -> String -> String
hillG ks = hillF (matrizInv ks)

-- ---------------------------------------------------------------------
-- Vamos a comprobar con QuickCheck la corrección del criptosistema:

-- matriz es un generador de matrices cuadradas. Por ejemplo,
--    λ> generate matriz
--    (  12  23 )
--    (   3 -12 )
--    λ> generate matriz
--    ( -24  27  15 )
--    ( -10  17 -16 )
--    (   3  30  19 )
matriz :: Gen (Matrix Int)
matriz = do n <- choose (2,6)
            xs <- suchThat (vector (n*n)) (esInvertible n)
            return (fromList n n xs)
    where esInvertible n xs = invertible (fromList n n xs) 

-- mensajeYclaveHill es un generador de mensajes y claves. Por ejemplo,
--    λ> generate mensajeYclaveHill
--    ("LXVUIQKTTJKBXDSB",
--    (  19  -5 -25 -19  27 )
--    (  17   0   0 -18  14 )
--    (   3 -14 -21  17  -9 )
--    (  25   8 -18  24  11 )
--    ( -20 -18  33  -4   1 ))
--    λ> generate mensajeYclaveHill
--    ("AMNEKOYVWNCOZ",
--    (  20 -17 -22   5 )
--    (  18  -3  30  32 )
--    (  20  10 -13  19 )
--    ( -23 -24  30   1 ))
mensajeYclaveHill :: Gen (String,Matrix Int)
mensajeYclaveHill = do m <- mensaje
                       k <- matriz
                       return (m,k)

-- Propiedad: Al cifrar un mensaje con hillF y descifrarlo con
-- hillG se obtiene el mensaje original.
prop_CorreccionHill :: Property
prop_CorreccionHill = 
    forAll mensajeYclaveHill 
           (\(m,k)-> m == hillG k (hillF k m))

-- La comprobación es
--    λ> quickCheck prop_CorreccionHill
--    +++ OK, passed 100 tests.
