-- Rabin.hs
-- Cifrado de Rabin.
-- Sevilla, Mayo de 2016
-- ---------------------------------------------------------------------
 
{-# LANGUAGE   PackageImports
             , ScopedTypeVariables 
             , NoMonomorphismRestriction #-}

module Rabin where
 
import Asociacion4 (strToInteger, integerToStr)
import Data.Char (ord)
import Codec.Crypto.RSA.Exceptions
import  "crypto-api" Crypto.Random
import Crypto.Random.DRBG
import Math.NumberTheory.Moduli (sqrtModFList)
import System.IO.Unsafe
import System.Random
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Vamos a implementar el criptosistema de Rabin
-- ---------------------------------------------------------------------

-- En primer lugar generaremos las claves pública y privada:

-- (choose' n) nos da un número aleatorio entre el 1 y el n. Por ejemplo,
--     ghci> choose' 10
--     4
--     ghci> choose' 10
--     5
--     ghci> choose' 10
--     3
choose' :: (Integral a, Random a) => a -> a
choose' n = unsafePerformIO (aux n)
    where aux m = getStdRandom (randomR (1,div m 2))

-- (prim3Mod4 m (p,g)) devuelve un primo de orden m, que es congruente
-- con 3 módulo 4. Es necesaria esta propiedad porque usamos el algoritmo
-- de Shanks para calcular las raíces cuadradas.
-- Nota: la función recibe de entrada un par (primo,generador de primos)
prim3Mod4 :: CryptoRandomGen t => Int -> (Integer, t) -> Integer
prim3Mod4 m (p,g) | p `mod` 4 == 3 = p
                  | otherwise = prim3Mod4 m x
    where x = largeRandomPrime g m

-- (clavesPubyPrivRabin m) devuelve un ejemplo de claves pública y privada
-- del criptosistema Rabin. Por ejemplo,
--     ghci> clavesPubyPrivRabin 3
--     ((236926650185801,85241385237504),
--      (15362491,15422411,236926650185801,85241385237504))
--     ghci> clavesPubyPrivRabin 3
--     ((194132976403109,141747768331538),
--      (13482731,14398639,194132976403109,141747768331538))
clavesPubyPrivRabin :: Int ->
              ((Integer,Integer),(Integer,Integer,Integer,Integer))
clavesPubyPrivRabin n = unsafePerformIO (aux n)
  where aux m = do
          g1 :: GenAutoReseed HashDRBG HashDRBG <- newGenIO
          g2 :: GenAutoReseed HashDRBG HashDRBG <- newGenIO
          let (p0,g3) = largeRandomPrime g1 m
              p       = prim3Mod4 m (p0,g3)
              (q0,g4) = largeRandomPrime g2 m
              q       = prim3Mod4 m (q0,g4)
              n'      = p*q
              e       = fromIntegral (2 * choose' n)
          return ((n',e),(p,q,n',e))

-- ---------------------------------------------------------------------
-- Ya podemos implementar las funciones encriptadora y desencriptadora:
-- ---------------------------------------------------------------------

-- (encriptaRabin x m) encripta el mensaje m usando el criptosistema
-- de Rabin de clave pública x.
encriptaRabin1 :: (Integer, t) -> String -> String
encriptaRabin1 (n,_) ms = integerToStr ((m*m) `mod` n)
  where m = strToInteger ms

-- (desencriptaRabin1 y m) desencripta el mensaje m usando el criptosistema
-- de Rabin de clave privada y.
desencriptaRabin1 :: (Integer, Integer, Integer, t) -> String -> [String]
desencriptaRabin1 (p,q,n,_) ms =
  foldr (\x y -> integerToStr (x `mod` n) : y) [] raices2
      where m       = strToInteger ms
            raices2 = sqrtModFList m [(p,1),(q,1)]

-- ---------------------------------------------------------------------
-- Ejemplo de encriptado y desencriptado:

--    ghci> let (x,y) = clavesPubyPrivRabin 10

--    ghci> encriptaRabin1 x "Pi?"
--    "\ACK\416\821v\233\969"
--    (0.00 secs, 1030560 bytes)
--    ghci> desencriptaRabin1 y it
--    ["\SOH\281\973\197\952\848\353\830\194D\885\357\385>\582\490\990",
--     "\628\457\345\756\810\454\358\853\914\817\522\993\646\178\290\286",
--     "\653\515\852\196%\899\471\340\154C\834\391\416\484\305\767",
--     "Pi?"]
-- ---------------------------------------------------------------------

-- Pero las funciones anteriores tienen un defecto, el número de entrada
-- no es muy grande asi que no supera a n. Por tanto, bastaria con hacer
-- la raiz cuadrada usual para obtener el mensaje original. Como es facil
-- de deducir esto es una gran debilidad del criptosistema.
-- Por tanto hemos implementado la siguiente variante:

-- (encriptaRabin x m) encripta el mensaje m usando el criptosistema
-- de Rabin de clave pública x. 
encriptaRabin :: (Integer, Integer) -> String -> String
encriptaRabin (n,e) ms = integerToStr ((m*(m+e)) `mod` n)
                          where m = strToInteger ms

-- (desencriptaRabin y m) desencripta el mensaje m usando el criptosistema
-- de Rabin de clave privada y.
desencriptaRabin
  :: (Integer, Integer, Integer, Integer) -> String -> [String]
desencriptaRabin (p,q,n,e) ms =
  foldr (\x y -> integerToStr ((x-e_2) `mod` n) : y) [] raices2
            where e2_4    = div (e * e) 4
                  e_2     = div e 2
                  m       = strToInteger ms
                  raices2 = sqrtModFList ((m + e2_4) `mod` n) [(p,1),(q,1)]

-- ---------------------------------------------------------------------
-- Ejemplos de encriptado y desencriptado:

--    ghci> let (x,y) = clavesPubyPrivRabin 10
--    (0.01 secs, 3091840 bytes)

--    ghci> encriptaRabin x "Pi?"
--    "\309\576\234\639\672\757\743\936\841\886\143\720\418\965\648\541"
--    (0.01 secs, 1029080 bytes)
--    ghci> desencriptaRabin y it
--    ["\613\739\202\906\732\981\655\623\838\195\729\212\362\883\ACK]",
--     "Pi?",
--     "-\201\891\563\318\681\570\937\466\962\377\281\809\360\982\788",
--     "\474\215\181\777\440\347\286\155\356\718\708\555\718\143\646["]
--    (0.01 secs, 2647864 bytes)

--    ghci> encriptaRabin x "rAbiN?"
--    "\732\534\187\873\800\197\596\551\506\609\208d\598\375\930\957"
--    (0.03 secs, 35302968 bytes)
--    ghci> desencriptaRabin y it
--    ["rAbiN?",
--     "\641\957\129\CAN\281Z\953\513\519\286\411\834\309\153\228\390",
--     "\736\770\452\219\518\855\443/\623\781\424\862\822\986\620\226",
--     "\149\786\878\b\313\866\373\418\381\382\756a5\688q\DLE"]
--    (0.01 secs, 31038215 bytes)
-- ---------------------------------------------------------------------

-- Vamos a comprobar con QuickCheck la corrección del criptosistema:

-- Implementamos una propiedad para verificar la corrección del
-- criptosistema para mensajes no vacíos, que no empiecen por el
-- carácter '\NUL' y de longitud acotada. Ademas tenemos la necesidad
-- de que el número entero correspondiente al mensaje m sea menor
-- que el entero n.

-- La propiedad queda:

prop_CorrecRabin :: Int -> String -> Property
prop_CorrecRabin x m =
    m /= [] && 0 < x && x < 30 && (ord (head m) > 0) &&
     (length m < div ((length . show) a) 3) ==>
      elem m (desencriptaRabin (c,d,e,f) (encriptaRabin (a,b) m))
    where ((a,b),(c,d,e,f)) = clavesPubyPrivRabin x

-- Que al ejecutar en consola:
--     ghci> quickCheck prop_CorrecRabin
--     +++ OK, passed 100 tests.
--     (13.76 secs, 15876675976 bytes)
