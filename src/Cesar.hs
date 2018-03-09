-- Cesar.hs
-- Cifrado César.
-- Sevilla, 25 de Noviembre de 2015
-- ---------------------------------------------------------------------
 
module Cesar where
 
import Asociacion1 (char2int, int2str, str2int)
import Criptoanalisis (frecMayores)
import Generadores (mensaje)
import Test.QuickCheck

-- El cifrado César es un tipo de cifrado por sustitución en el que una
-- letra en el texto original es reemplazada por otra letra que se
-- encuentra un número fijo de posiciones más adelante en el alfabeto. 
 
-- (cesarF k m) es el mensaje obtenido cifrando m mediante el código
-- César con desplazamiento k. Por ejemplo,
--    cesarF 3 "TODOPARANADA"  ==  "WRGRSDUDQDGD"
cesarF :: Int -> String -> String
cesarF k m = int2str [(n+k) `mod` 26 | n <- str2int m]
 
-- (cesarG k m') es el mensaje obtenido descifrando m' mediante el código
-- César con desplazamiento k. Por ejemplo,
--    cesarG 3 "WRGRSDUDQDGD"  ==  "TODOPARANADA"
cesarG :: Int -> String -> String
cesarG k = cesarF (-k)

------------------------------------------------------------------------

-- Vamos a comprobar con QuickCheck la corrección del criptosistema:

-- Propiedad: Al cifrar un mensaje con cesarF y descrifrarlo con
-- cesarG se obtiene el mensaje original.
prop_CorreccionDeCesar :: Int -> Property
prop_CorreccionDeCesar k = 
   k >= 0 ==>
   forAll mensaje (\m -> m == cesarG k (cesarF k m))

-- La comprobación es
--    ghci> quickCheck prop_CorreccionDeCesar
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Criptoanálisis
-- ---------------------------------------------------------------------

-- El criptosistema César es muy fácil de atacar, puesto que sólo hay 26
-- claves posibles (que en criptografia son muy pocas) asi que se puede
-- intentarlo con todas y ver si algun texto tiene sentido. 
-- Sin embargo, este método se puede pulir un poco. Basta fijarse en las
-- letras más repetidas del texto que queremos desencriptar y tratar de
-- buscar la clave para que coincida con la más repetida de nuestro
-- idioma. Por ejemplo, tenemos un texto encriptado en el que se
-- repite mucho la letra 'G'. Sabemos que, tanto en el inglés como en
-- el castellano, la letra más usada es la 'E'. Así que una probable
-- clave es el número:  
--    char2int('G') - char2int('E') = 6 - 4 = 2

-- (desencriptaCesarBruta m) es la lista de los mensaje obtenidos
-- desencriptando m con los 26 desplazamientos. Por ejemplo,
--    ghci> take 4 (desencriptaCesarBruta "WRGRSDUDQDGD")
--    ["WRGRSDUDQDGD","VQFQRCTCPCFC","UPEPQBSBOBEB","TODOPARANADA"]
desencriptaCesarBruta :: String -> [String]
desencriptaCesarBruta m = [cesarG k m | k <- [0..25]]

-- (desencriptaCesarBruta m) es la lista de los mensaje obtenidos
-- desencriptando m de forma que las letras más frecuentes se
-- identifique con la 'E'. Por ejemplo,
--    ghci> desencriptaCesarFino "QTGZJSTXNGWJAJITXAJHJXGZJST"
--    ["LOBUENOSIBREVEDOSVECESBUENO"]
desencriptaCesarFino :: String -> [String]
desencriptaCesarFino m = 
    [cesarG (char2int k - 4) m | k <- frecMayores m]
