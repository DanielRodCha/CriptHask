-- Vigenere.hs
-- Cifrado Vigenère.
-- Sevilla, 25 de Noviembre de 2015
-- ---------------------------------------------------------------------

module Vigenere where
 
import Asociacion1 (str2int, int2char)
import Generadores (genLetra)
import Test.QuickCheck

-- repite :: String -> Int -> String 
-- repite xs 0 = []
-- repite xs n = xs ++ (repite xs (n-1))

repite :: String -> String
repite = concat . repeat

-- (vigenereF k m) es el mensaje m encriptado por el método de Vigenère
-- con clave k. Por ejemplo,
--    vigenereF "CLAVE" "ENTODOLAMEDIDA"  ==  "GYTJHQWAHIFTDV"
--    vigenereF "A"     "ENTODOLAMEDIDA"  ==  "ENTODOLAMEDIDA"
--    vigenereF "B"     "ENTODOLAMEDIDA"  ==  "FOUPEPMBNFEJEB"
vigenereF :: String -> String -> String
vigenereF k m = map (\x -> int2char (x `mod` 26)) (zipWith (+) xs ys)
    where xs = str2int m
          ys = str2int (repite k)

-- (vigenereG k m) es el mensaje m desencriptado por el método de Vigenère
-- con clave k. Por ejemplo,
--    vigenereG "CLAVE" "GYTJHQWAHIFTDV"  ==  "ENTODOLAMEDIDA"
vigenereG :: String -> String -> String
vigenereG k m' = map (\x -> int2char (x `mod` 26)) (zipWith (-) xs ys)
    where xs = str2int m'
          ys = str2int (repite k)

-- Vamos a comprobar con QuickCheck la corrección del criptosistema:

-- mensajeOclaveV es un generador de mensajes o claves (i.e. cadenas de
-- letras mayúsculas). Por ejemplo, 
--    generate mensajeOclaveV  ==  "DLUJLUGSACBE"
--    generate mensajeOclaveV  ==  "KEROEIOYGYJWAWAKJFJ"
--    generate mensajeOclaveV  ==  "YETMXNKLPX"
mensajeOclaveV :: Gen String
mensajeOclaveV = listOf1 genLetra

-- Propiedad: Al cifrar un mensaje con vigenereF y descrifrarlo con
-- vigenereG se obtiene el mensaje original.
prop_CorreccionVigenere :: Property
prop_CorreccionVigenere = 
    forAll mensajeOclaveV 
           (\k -> forAll mensajeOclaveV
                         (\m -> m == vigenereG k (vigenereF k m)))

-- La comprobación es
--    ghci> quickCheck prop_CorreccionVigenere
--    +++ OK, passed 100 tests.
-- ---------------------------------------------------------------------


