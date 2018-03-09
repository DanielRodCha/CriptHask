module Asociacion3 where

-- (bool2int b) es la codificación entera del booleano b.
bool2int :: Bool -> Int
bool2int True  = 1
bool2int False = 0

-- (xor xs ys) es la disyunción excluyente de los correspondientes
-- elementos de xs e ys. Por ejemplo,
--    xor [0,1,0,0,0,0,0,1] [0,1,0,0,0,0,0,1]  ==  [0,0,0,0,0,0,0,0]
--    xor [0,1,0,0,0,0,0,1] [0,1,0,0,0,0,1,0]  ==  [0,0,0,0,0,0,1,1]
xor :: [Int] -> [Int] -> [Int]
xor xs ys = [xor' x y | (x,y) <- zip xs ys]
            where xor' x y = bool2int (x /= y)

-- (int2bin4' n) es el número binario correspondiente al decimal n. Por
-- ejemplo, 
--    int2bin4' 65  ==  [1,0,0,0,0,0,1]
--    int2bin4' 66  ==  [1,0,0,0,0,1,0]
int2bin4' :: Integral a => a -> [a]
int2bin4' n | n < 2     = [n]
            | otherwise = int2bin4' (n `div` 2) ++ [n `mod` 2]

-- (largo4 xs) añade ceros al comienzo de xs hasta que la longitud sea
-- 4 (se supone que la longitud de xs es menor o igual que 4). Por
-- ejemplo, 
--    largo4 [3,7]  ==  [0,0,3,7]
largo4 :: Num a => [a] -> [a]
largo4 xs | length xs >= 4 = xs
          | otherwise      = largo4 (0:xs)

-- (int2bin4 n) es el número binario correspondiente al decimal n (con
-- 4 elementos como mínimo). Por ejemplo, 
--    int2bin4 5   ==  [0,1,0,1]
--    int2bin4 65  ==  [1,0,0,0,0,0,1]
int2bin4 :: Integral a => a -> [a]
int2bin4 = largo4 . int2bin4'

-- (bin2int4 ns) es el número decimal correspondiente al binario ns
-- (con sus dígitos en orden inverso). Por ejemplo, 
--    bin2int [0,1,0,0,0,0,0,1]  ==  65
bin2int4 :: [Int] -> Int
bin2int4 xs = foldr (\x y -> x + 2*y) 0 (reverse xs)

-- Propiedad: Al pasar un número natural a binario con int2bin4 y el
-- resultado a decimal con bin2int4 se obtiene el número inicial.
prop_int2bin4 :: Int -> Bool
prop_int2bin4 x =
   bin2int4 (int2bin4 y) == y
    where y = abs x


