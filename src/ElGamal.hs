-- ElGamal.hs
-- Cifrado de El Gamal.
-- Sevilla, Mayo de 2016
-- ---------------------------------------------------------------------
 
{-# LANGUAGE   PackageImports
             , ScopedTypeVariables 
             , NoMonomorphismRestriction #-}

module ElGamal where
 
import Asociacion4 (strToInteger, integerToStr, expQuickMod)
import Data.Char (ord)
import Data.Text (chunksOf,pack,unpack)
import Codec.Crypto.RSA.Exceptions
import  "crypto-api" Crypto.Random
import Crypto.Random.DRBG
-- import Data.Numbers.Primes (primeFactors)
import Factory.Math.PrimeFactorisation
import Factory.Math.Implementations.PrimeFactorisation
import Test.QuickCheck
import System.IO.Unsafe

-- ---------------------------------------------------------------------
-- Vamos a implementar el criptosistema de El Gamal

-- En el cifrado de El Gamal, todos los usuarios del sistema comparten
-- cierta información que llamaremos entorno, formado por:
--    *) Un cierto primo p que genera el cuerpo F_p
--    *) Otro primo q de forma que q|(p-1)
--    *) Una base g de el grupo multiplicativo de F_p que tenga orden
--       divisible por q. O, equivalentemente, que verifique:
--           g^((p-1)/q) `mod` p /= 1

-- (baseG p q) genera una base g con las propiedades descritas
-- anteriormente. Por ejemplo,
--     ghci> baseG 7 2
--     3
--     ghci> baseG 7 2
--     5
--     ghci> baseG 7 3
--     4
baseG :: Integer -> Integer -> Integer
baseG p q = unsafePerformIO (generate (baseG' p q))
  where baseG' a b = suchThat (choose (1,a-1))
                        (\x -> 1 /= expQuickMod x (div (a-1) b) a)
  
-- (generaEntorno n) nos genera un ejemplo de entorno para encriptar.
-- Por ejemplo,
--     ghci> generaEntorno 2
--     (56659,71,46086)
--     ghci> generaEntorno 5
--     (867682941677,2662813,566898953861)
--     ghci> generaEntorno 8
--     (15885909102595013579,1150986023952689,14361151548208316332)
--     ghci> generaEntorno 10
--     (909209565115226705262277,17530839358873691,803233093626684455375681)
generaEntorno :: Int -> (Integer, Integer, Integer)
generaEntorno m = unsafePerformIO (generaEntorno' m)
  where generaEntorno' n = do
          g1 :: GenAutoReseed HashDRBG HashDRBG <- newGenIO
          let (p,_) = largeRandomPrime g1 n
              (q,_) = last (primeFactors TrialDivision (p-1))
              g     = baseG p q
          return (p,q,g)

{-
generaEntorno :: Int -> (Integer, Integer, Integer)
generaEntorno m = unsafePerformIO (generaEntorno' m)
  where generaEntorno' m = do
          g1 :: GenAutoReseed HashDRBG HashDRBG <- newGenIO
          let (p,g2)  = largeRandomPrime g1 m
              q       = last (primeFactors (p-1))
              g       = baseG p q
          print (m,q)
          return (p,q,g)
-}

-- NOTA: el cálculo de q es poco eficiente, pero debe ser grande pues
-- la seguridad del sistema depende de esto. Con el uso de la librería
-- factory lo hemos intentado arreglar pero la implementación del
-- método de Fermat es aún más lenta:

--     ghci> primeFactors TrialDivision 23515022352
--     [(2,4),(3,1),(489896299,1)]
--     (0.02 secs, 11952264 bytes)
--     ghci> primeFactors FermatsMethod 23515022352
--     (no acaba)

-- Para generar una clave pública el usuario escoge un entero n con
-- la única condición de que debe ser menor que p-1. Luego calcula
--        e = g^n `mod` p
-- (clavePubyPrivElGamal x) genera las claves pública y privada para el
-- criptosistema de El Gamal a partir de un entorno x. Por ejemplo,
--     ghci> clavePubyPrivElGamal (13061663,6530831,4826906)
--     ((13061663,4826906,5648996),(13061663,697850))
--     ghci> clavePubyPrivElGamal (13061663,6530831,4826906)
--     ((13061663,4826906,691985),(13061663,3930900))
clavePubyPrivElGamal :: (Integer, t, Integer)
                     -> ((Integer, Integer, Integer),(Integer,Integer))
clavePubyPrivElGamal x = unsafePerformIO (clavePubyPrivElGamal' x)
  where clavePubyPrivElGamal' (p,_,g) = do
            let n = unsafePerformIO (generate (choose (1,p-1)))
            return ((p,g,expQuickMod g n p),(p,n))

-- A la hora de encriptar el mensaje m usaremos una clave efímera h
-- (un entero cualquiera) y realizamos la siguiente operación:
-- m' = (m1,m2) = (g^h `mod` p,m*(e^h) `mod` p)
-- Nota1: escogeremos h entre 1 y p por simplicidad.
-- (encriptaElGamal m (a,b,c)) encripta el mensaje m con el criptosistema
-- de EL Gamal usando la clave pública (a,b,c).

encriptaElGamal :: String
                   -> (Integer, Integer, Integer) -> [(String, String)]
encriptaElGamal m (a,b,c) = foldr
                   (\z y-> encriptaElGamal' (unpack z) (a,b,c) : y) []
                           (chunksOf (div ((length . show) a) 3) (pack m))
 where encriptaElGamal' x y = unsafePerformIO  (encriptaElGamal'' x y)
        where encriptaElGamal'' mensaje (p,g,e) = do
               let h = unsafePerformIO (generate (choose (1,p)))
               return (integerToStr (expQuickMod g h p),
                       integerToStr (m' * expQuickMod e h p `mod` p))
               where m' = strToInteger mensaje

-- (desencriptaElGamal m (a,b)) desencripta el mensaje m encriptado
-- con el criptosistema de El Gamal usando la clave privada (a,b).
desencriptaElGamal :: [(String, String)] -> (Integer, Integer) -> [Char]
desencriptaElGamal xs (a,b) = foldr
             (\m y -> (desencriptaElGamal' m (a,b) ++ y)) [] xs
 where desencriptaElGamal' (mensaje1,mensaje2) (p,n) =
          integerToStr (expQuickMod m1 (p-1-n) p * m2 `mod` p)
             where (m1,m2) = (strToInteger mensaje1, strToInteger mensaje2)

-- ---------------------------------------------------------------------
-- Vamos a comprobar con QuickCheck la corrección del criptosistema:

-- Implementamos una propiedad para verificar la corrección del
-- criptosistema para mensajes no vacíos y que no contengan el
-- carácter '\NUL'.

-- La función generaEntorno es muy ineficiente así que vamos a verificar
-- la misma propiedad de dos formas distintas:


-- En primer lugar el mensaje se dividirá en sub-mensajes de longitud 8,
-- pero mantenemos fijo el entorno. Por tanto, la primera propiedad queda,
prop_CorrecElGamal1 :: String -> Property
prop_CorrecElGamal1 m =
  m /= [] && all (\ x -> ord x > 0) m ==>
    m == desencriptaElGamal (encriptaElGamal m clavPub) clavPriv
      where (clavPub,clavPriv) =
             clavePubyPrivElGamal (909209565115226705262277 :: Integer,
                                   17530839358873691 :: Integer,
                                   803233093626684455375681 :: Integer)

-- En segundo lugar vamos a ir variando entre distintos entornos
-- de tamaño n.
prop_CorrecElGamal2 :: Int -> String -> Property
prop_CorrecElGamal2 n m =
       m /= [] && all (\ x -> ord x > 0) m  ==>
       m == desencriptaElGamal (encriptaElGamal m clavPub) clavPriv
         where (clavPub,clavPriv) = clavePubyPrivElGamal (generaEntorno n)

-- Ambas comprobaciones son:

--     ghci> quickCheck prop_CorrecElGamal1
--     +++ OK, passed 100 tests.
--     (0.36 secs, 178844768 bytes)

--     ghci> quickCheck (prop_CorrecElGamal2 5)
--     +++ OK, passed 100 tests.
--     (3.34 secs, 4208132400 bytes)
