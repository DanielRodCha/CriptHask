module Asociacion4 where

import Data.Char (ord,chr)
import Data.List.Extra (chunksOf)    
import Test.QuickCheck

-- (strToInteger m) hace corresponder un mensaje en mayúsculas con un entero
-- muy grande. Para ello transforma cada letra en su código ASCII y
-- concatena todos los números en uno. Hay que destacar que el código ASCII
-- de un carácter es un número de 3 cifras, claro que los ceros a la izquierda
-- se omiten (065 == 65).
--    strToInteger "A"          == 65
--    strToInteger "AA"         == 65065
--    strToInteger "ABCDEFGHIJ" == 65066067068069070071072073074
strToInteger1 :: String -> Integer
strToInteger1 cs =
    foldl (\x y -> toInteger y + 1000 * toInteger x) 0 (map ord cs)

-- Definicion alternativa:
strToInteger :: String -> Integer
strToInteger = read . concatMap aux
    where aux c | c < '\n' = "00" ++ cs
                | c < 'd'  = '0' : cs 
                | otherwise = cs             
                where cs = show (ord c)

-- Comprobemos que ambas definiciones son iguales:
prop_strToInteger :: String -> Property
prop_strToInteger m =
     m /= [] ==> strToInteger1 m == strToInteger m

-- La comprobación es:
--     ghci> quickCheck prop_strToInteger
--     +++ OK, passed 100 tests.

--  Comparación de eficiencia
--    ghci> length (show (strToInteger1 (replicate (10^5) 'A')))
--    299999
--    (18.03 secs, 12507339616 bytes)
--    ghci> length (show (strToInteger (replicate (10^5) 'A')))
--    299999
--    (6.60 secs, 37526646896 bytes)

-- integerToStr es la inversa de strToInteger; es decir, (integerToStr n)
-- es la cadena cs tal que (strToInteger cs) es n.
--    integerToStr 65                            == "A"
--    integerToStr 65065                         == "AA"
--    integerToStr 65066067068069070071072073074 == "ABCDEFGHIJ"
integerToStr1 :: Integer -> String
integerToStr1 0 = []
integerToStr1 n = integerToStr1 (div n 1000)
                  ++ [chr (fromIntegral (rem n 1000))]

-- Definicion alternativa:
integerToStr :: Integer -> String
integerToStr m | x == 0    = aux s
               | x == 1    = chr (read (take 1 s)) : aux (drop 1 s)
               | otherwise = chr (read (take 2 s)) : aux (drop 2 s)
    where x     = rem (length (show m)) 3
          s     = show m
          aux n = map (chr . read) (chunksOf 3 n)

-- Comprobemos que ambas definiciones son iguales:
prop_integerToStr :: Integer -> Property
prop_integerToStr n =
     n>0 ==>  integerToStr1 n == integerToStr n

-- La comprobación es:
--    ghci> quickCheck prop_integerToStr
--    +++ OK, passed 100 tests.

-- Veamos ahora que una función es inversa de la otra:
prop_strToInteger_integerToStr :: String -> Property
prop_strToInteger_integerToStr m =
     m /= [] && 0 /= (ord . head) m ==>
                 m == (integerToStr . strToInteger) m

prop_integerToStr_strToInteger :: Integer -> Property
prop_integerToStr_strToInteger n =
     n > 0 ==> n == (strToInteger . integerToStr) n

-- Las comprobaciones son:
--     ghci> quickCheck prop_strToInteger_integerToStr
--     +++ OK, passed 100 tests.
--     ghci> quickCheck prop_integerToStr_strToInteger
--     +++ OK, passed 100 tests.

-- Comentario: La propiedad prop_strToInteger_integerToStr falla. Por
-- ejemplo,
--    λ> quickCheck prop_strToInteger_integerToStr
--    *** Failed! Falsifiable (after 69 tests and 6 shrinks): 
--    "\NULa"
-- En efecto,
--    λ> let m = "\NULa"
--    λ> m /= []
--    True
--    λ> strToInteger m
--    97
--    λ> integerToStr it
--    "a"
--    λ> m == (integerToStr . strToInteger) m
--    False
-- En general, el problema lo da las cadenas que empiecen con caracteres
-- nulos, ya que su código ASCII es 0; por ejemplo,
--    λ> strToInteger "\NUL"
--    0
--    λ> strToInteger "\NUL\NUL"
--    0
--    λ> strToInteger "\NUL\NULa"
--    97
--    λ> strToInteger "\NULa"
--    97
--    λ> strToInteger "a"
--    97

-- (expQuickMod x e n) calcula x^e módulo n de una forma eficiente.
-- Algunos ejemplos son:
--    expQuickMod 2 2 4    == 0
--    expQuickMod 2 2 5    == 4
--    expQuickMod 7 256 58 == 23
expQuickMod :: Integer -> Integer -> Integer -> Integer
expQuickMod x 1 n = x `mod` n
expQuickMod x e n | even e    = expQuickMod (x*x `mod` n) (div e 2) n
                  | otherwise = (x * expQuickMod x (e-1) n) `mod` n

-- Comprobemos que la definición es correcta:
prop_Exp :: Integer -> Integer -> Integer -> Property
prop_Exp x e n = 
    e > 0 && n > 0 ==>
    expQuickMod x e n == x^e `mod` n

--  La comprobación es
--    ghci> quickCheck prop_Exp
--    +++ OK, passed 100 tests.

-- (coprimos a b) se verifica si a y b son primos entre sí. Por ejemplo,
--    coprimos 4 15  ==  True
--    coprimos 4 10  ==  False
coprimos :: Integral a => a -> a -> Bool
coprimos a b = gcd a b == 1
