-- Criptoanalisis.hs
-- Sevilla, 23 de Febrero de 2016
-- ---------------------------------------------------------------------
 
module Criptoanalisis where
 
import Asociacion1 (abecedario)
import Data.List (isPrefixOf, sortBy)

-- (aparicion a xs) es el numero de veces que está la letra a en
-- la cadena xs. Por ejemplo,
--    aparicion 'A' "AMAZONAS" == 3
aparicion :: Char -> String -> Int
aparicion a xs = length (filter (==a) xs)

-- (frecuencias m) es lalista de apariciones de cada letra del abecedario
-- en el mensaje m. Por ejemplo,
--    ghci> frecuencias "TODOPARANADA"
--    [(4,'A'),(0,'B'),(0,'C'),(2,'D'),(0,'E'),(0,'F'),(0,'G'),(0,'H'),
--     (0,'I'),(0,'J'),(0,'K'),(0,'L'),(0,'M'),(1,'N'),(2,'O'),(1,'P'),
--     (0,'Q'),(1,'R'),(0,'S'),(1,'T'),(0,'U'),(0,'V'),(0,'W'),(0,'X'),
--     (0,'Y'),(0,'Z')]
frecuencias :: String -> [(Int,Char)]
frecuencias m = [(aparicion a m,a) | a <- abecedario]

-- (ordena2 xs) recibe una lista de pares y la ordena de mayor a menor,
-- extrayendo el segundo miembro de cada par. Finalmente nos quedan los
-- segundos miembros de los pares ordenados en función de los primeros.
-- Por ejemplo:
--    ordena2 [(5,'A'),(2,'B'),(3,'C'),(2,'D'),(7,'E')] == "EACDB"
ordena2 :: [(Int,Char)] -> String
ordena2 xs = map snd (sortBy (flip compare) xs)

-- (frecMayores m) es la lista con las letras más repetidas. Por ejemplo, 
--    frecMayores "TODOPARANADA"  ==  "A"
frecMayores :: String -> String
frecMayores m = [y | (x,y) <- frecuencias m, x == frecMayor]
    where frecMayor = maximum [aparicion a m | a <- abecedario]

-- (aparicion2 xs ys) es el número de ocurrencias de xs en ys. Por ejemplo,
--    aparicion2 "ac" "acbacd"  ==  2
aparicion2 :: String -> String -> Int
aparicion2 _ [] = 0
aparicion2 xs (y:ys) 
   | xs `isPrefixOf` (y : ys) = 1 + aparicion2 xs ys
   | otherwise                = aparicion2 xs ys
