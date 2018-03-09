-- RSA.hs
-- Cifrado RSA.
-- Sevilla, Abril de 2016
-- ---------------------------------------------------------------------
 
{-# LANGUAGE   PackageImports
             , ScopedTypeVariables 
             , NoMonomorphismRestriction #-}

module RSA where

import Asociacion1 (invMod)
import Asociacion4 (strToInteger, integerToStr, coprimos, expQuickMod)
import Codec.Crypto.RSA.Exceptions
import  "crypto-api" Crypto.Random
import Crypto.Random.DRBG
import Data.Char (ord)
import System.IO.Unsafe
import Test.QuickCheck
    
-- ---------------------------------------------------------------------
-- Vamos a implementar el Criptosistema RSA

-- (ejemploE n) es un generador de números entre 1 y n que son primos
-- con n. Por ejemplo,
--    ghci> generate (ejemploE 125)
--    24
--    ghci> generate (ejemploE 125)
--    116
--    ghci> generate (ejemploE 125)
--    92
ejemploE :: Integer -> Gen Integer
ejemploE phiN = 
    suchThat (choose (1,phiN)) (coprimos phiN)

-- (ejemploE' n) simplemente extrae de la mónada la solución
-- de (ejemploE n). Por ejemplo,
--     ghci> ejemploE' 125
--     16
--     ghci> ejemploE' 125
--     2
--     ghci> ejemploE' 125
--     124
ejemploE' :: Integer -> Integer
ejemploE' phiN =
    unsafePerformIO (generate (ejemploE phiN))

-- (clavePubYPrivRSA m) genera una mónada que contiene un par con las
-- claves públicas y privadas. Por ejemplo,
--    λ> clavePubYPrivRSA 2
--    ((3103581713,1723633031),(3103581713,127673339))
--    λ> clavePubYPrivRSA 2
--    ((2895506491,2494485681),(2895506491,2820906505))
clavePubYPrivRSA :: Int -> IO ((Integer, Integer),(Integer, Integer))
clavePubYPrivRSA m = do
  g1 :: GenAutoReseed HashDRBG HashDRBG <- newGenIO
  let (p,g2) = largeRandomPrime g1 m
      (q,_)  = largeRandomPrime g2 m
      n = p*q
      phiN = (p-1)*(q-1)
      e = ejemploE' phiN
      d = invMod e phiN
  return ((n,e),(n,d))

-- (clavePubYPrivRSA' m) únicamente extrae, de forma insegura, de la mónada
-- obtenida en la funcion anterior, las claves que usaremos. Por
-- ejemplo, 
--    ghci> clavePubYPrivRSA' 1
--    ((3773316689,843897733),(3773316689,2102173897))
--    ghci> clavePubYPrivRSA' 1
--    ((3170052121,2241654313),(3170052121,2862005325))
--    ghci> clavePubYPrivRSA' 3
--    ((172463926379321,1394429464811),(172463926379321,132948730937291))
clavePubYPrivRSA' :: Int -> ((Integer, Integer), (Integer, Integer))
clavePubYPrivRSA' m =
    unsafePerformIO (clavePubYPrivRSA m)

-- (encriptaRSA m (n,e)) encripta el mensaje m con la clave pública
-- (n,e) realizando la siguiente operación:
--    encriptaRSA m (n,e) == M^e `mod` n
-- donde M es el entero correspondiente al mensaje m. Por ejemplo,
--    λ> encriptaRSA "EVA" (3283788779,984381157)
--    "\864\572\808"
encriptaRSA ::  String -> (Integer,Integer) -> String
encriptaRSA m (n,e) = integerToStr (expQuickMod (strToInteger m) e n)

-- Observación: el entero (strToInteger m) debe ser más pequeño que n.

-- El proceso de desencriptado del RSA es análogo al de encriptado,
-- con la diferencia que usa la clave privada. Por ejemplo,
--    λ> desencriptaRSA "\864\572\808" (3283788779,1946330557)
--    "EVA"
desencriptaRSA :: String -> (Integer,Integer) -> String
desencriptaRSA = encriptaRSA
                 
-- ---------------------------------------------------------------------
-- Vamos a comprobar con QuickCheck la corrección del criptosistema:



-- Implementamos una propiedad para verificar la corrección del
-- criptosistema para mensajes no vacíos, que no empiecen por el
-- carácter '\NUL' y de longitud acotada.

-- Como ya hemos comentado, tenemos la necesidad de que el número entero
-- correspondiente al mensaje m sea menor que el entero n (por el que
-- hacemos módulo) para garantizar inyectividad de la función encriptadora.
-- Por tanto la cota depende de n.

-- x es el entero que define el tamaño de las claves y lo vamos a
-- acotar por 30

-- La propiedad es
prop_CorrecRSA :: Int -> String -> Property
prop_CorrecRSA x m =
    m /= [] && x>0 && x<30 && (ord (head m) > 0) &&
     (length m < div ((length . show) a) 3) ==>
      m == desencriptaRSA (encriptaRSA m (a,b)) (c,d)
    where ((a,b),(c,d)) = clavePubYPrivRSA' x

-- La comprobación es
--    ghci> quickCheck prop_CorrecRSA
--    +++ OK, passed 100 tests.
--    (8.80 secs, 9869952232 bytes)
--    (2.44 secs, 5,155,364,224 bytes)
