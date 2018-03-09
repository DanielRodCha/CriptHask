-- Asociacion1.hs
-- Funciones auxiliares.
-- Sevilla, 25 de Noviembre de 2015
-- ---------------------------------------------------------------------
 
module Asociacion1 where
 
-- Pdte. Descripción.

import Data.Char (chr, ord)
import Test.QuickCheck
 
-- (int2char n) es la n-ésima letra mayúscula. Por ejemplo,
--    int2char  0  ==  'A'
--    int2char  1  ==  'B'
--    int2char  2  ==  'C'
--    int2char 25  ==  'Z'
int2char :: Int -> Char
int2char n = chr (n + 65) -- ord('A') = 65
 
-- (chr2int c) es la posición de la letra mayúscula c. Por ejemplo,
--    char2int 'A'  ==  0
--    char2int 'B'  ==  1
--    char2int 'C'  ==  2
--    char2int 'Z'  ==  25
char2int :: Char -> Int
char2int c = ord c - 65
 
-- (int2str ns) es la cadena de letras mayúsculas correspondiente a la
-- lista de posiciones ns. Por ejemplo,
--    int2str [0,2,1,25]  ==  "ACBZ"
int2str :: [Int] -> String
int2str = map int2char
 
-- (str2int cs) es la lista de posiciones correspondiente a la cadena de
-- letras mayúsculas cs. Por ejemplo,
--    str2int "ACBZ"  ==  [0,2,1,25]
str2int :: String -> [Int]
str2int = map char2int

-- Definimos la lista "abecedario", que contiene a las letras mayúsculas
-- menos la letra Ñ. 
abecedario :: String
abecedario = ['A'..'Z']

-- (enAbecedario cs) se verifica si cs es una cadena no vacía de
-- letras mayúsculas excepto la Ñ. Por ejemplo: 
--    enAbecedario "HOLA" == True
--    enAbecedario "hola" == False
--    enAbecedario "AÑOS" == False
enAbecedario :: String -> Bool
enAbecedario [] = False
enAbecedario xs = all (`elem` abecedario) xs

-- [Algoritmo extendido de Euclides]
-- (mcd a b) es la terna (x,y,g) tal que g es el máximo común divisor
-- de a y b y se cumple que ax + by = g. Por ejemplo,
--    mcdExt 12 15  ==  (-1,1,3)
mcdExt :: Integral a => a -> a -> (a,a,a)
mcdExt a 0 = (1, 0, a)
mcdExt a b = (t, s - q * t, g)
    where (q, r)    = a `quotRem` b
          (s, t, g) = mcdExt b r

-- (invMod a n) es el inverso de a módulo n, verificando que a sea
-- unidad. De esta forma, se verifica que 
--    a * (invMod a n) `mod` n == 1.
-- Por ejemplo,
--    invMod 2 5 == 3 
-- pues 2*3 = 6, que es 1 módulo 5
invMod :: Integral a => a -> a -> a
invMod a m | x < 0     = x + m
           | otherwise = x
    where (x, _, _) = mcdExt a m

-- 2ª definición
invmod1 :: Integer -> Integer -> Integer
invmod1 a n
  | 1 == gcd a n = head [m | m <- [1..n-1], m*a `mod` n == 1]
  | otherwise    = error "No existe"

prop1_invMod :: Positive Integer -> Integer -> Property
prop1_invMod (Positive a) n =
    n > 1 && gcd a n == 1 ==> invMod a n == invmod1 a n

prop2_invMod1 :: Positive Integer -> Integer -> Property
prop2_invMod1 (Positive a) n =
    n > 1 && gcd a n == 1 ==> 
          0 <= x && x < n &&
          x * a `mod` n == 1 
    where x = invMod a n
