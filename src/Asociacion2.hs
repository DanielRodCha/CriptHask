module Asociacion2 where

import Data.Char (ord,chr)

-- (bin2int x) es el número decimal correspondiente al número binario
-- x. Por ejemplo, 
--    bin2int [1,0,1,1]  ==  13
bin2int :: [Int] -> Int
bin2int =  foldr (\x y -> x + 2*y) 0
 
-- Puede definirse por recursión
bin2intR :: [Int] -> Int
bin2intR [] = 0
bin2intR (x:xs) = x + 2 * bin2intR xs
 
-- Puede definirse por comprensión
bin2intC :: [Int] -> Int
bin2intC xs = sum [x*2^n | (x,n) <- zip xs [(0::Int)..]]
 
-- (int2bin x) es el número binario correspondiente al número decimal
-- x. Por ejemplo, 
--    int2bin 13  ==  [1,0,1,1]  
int2bin :: Int -> [Int]
int2bin n | n < 2     = [n]
          | otherwise = n `mod` 2 : int2bin (n `div` 2)
 
-- Propiedad: Al pasar un número natural a binario con int2bin y el
-- resultado a decimal con bin2int se obtiene el número inicial.
prop_int2bin :: Int -> Bool
prop_int2bin x =
   bin2int (int2bin y) == y
    where y = abs x
 
-- Comprobación:
--    > quickCheck prop_int2bin
--    +++ OK, passed 100 tests.

-- Codificación y descodificación
-- ==============================
 
-- Un octeto es un grupo de ocho bits.
 
-- (creaOcteto bs) es el octeto correspondiente a la lista de bits bs;
-- es decir, los 8 primeros elementos de bs si su longitud es mayor o
-- igual que 8 y la lista de 8 elemento añadiendo ceros al final de bs
-- en caso contrario. Por ejemplo, 
--    creaOcteto [1,0,1,1,0,0,1,1,1,0,0,0]  ==  [1,0,1,1,0,0,1,1]
--    creaOcteto [1,0,1,1]                  ==  [1,0,1,1,0,0,0,0]
creaOcteto :: [Int] -> [Int]
creaOcteto bs =  take 8 (bs ++ repeat 0)

-- creaOcteto se puede definir sin usar repeat:
creaOcteto' :: [Int] -> [Int]
creaOcteto' bs =  take 8 (bs ++ replicate 8 0)
 
-- (codifica c) es la codificación de la cadena c como una lista de bits
-- obtenida convirtiendo cada carácter en un número Unicode,
-- convirtiendo cada uno de dichos números en un octeto y concatenando
-- los octetos para obtener una lista de bits. Por ejemplo, 
--    *Main> codifica "abc"
--    [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
codifica :: String -> [Int]
codifica =  concatMap (creaOcteto . int2bin . ord)
 
-- (separaOctetos bs) es la lista obtenida separando la lista de bits bs
-- en listas de 8 elementos. Por ejemplo, 
--    *Main> separaOctetos [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0]
--    [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0]]
separaOctetos :: [Int] -> [[Int]]
separaOctetos [] = []
separaOctetos bs =  
    take 8 bs : separaOctetos (drop 8 bs)
 
-- (descodifica bs) es la cadena correspondiente a la lista de bits
-- bs. Por ejemplo,   
--    *Main> descodifica [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
--    "abc"
descodifica :: [Int] -> String
descodifica =  map (chr . bin2int) . separaOctetos
