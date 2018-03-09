module Generadores where

import Asociacion1 (abecedario)
import Test.QuickCheck

-- genLetra es un generador de letras mayúsculas. Por ejemplo,
--    generate genLetra  ==  'X'
--    generate genLetra  ==  'H'
--    generate genLetra  ==  'O'
genLetra :: Gen Char
genLetra = elements abecedario

-- mensaje es un generador de mensajes (i.e. cadenas de letras
-- mayúsculas). Por ejemplo, 
--    generate mensaje  ==  "CGS"
--    generate mensaje  ==  "TNFQEJ"
--    generate mensaje  ==  "QAJFMAN"
mensaje :: Gen String
mensaje = listOf genLetra
