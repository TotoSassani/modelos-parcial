{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import Data.Char ( toUpper )
data Barbaros = Barbaros{
    nombre::String,
    fuerza::Float,
    habilidades::[String],
    objetos::[String]
} deriving (Eq,Show)

type Objetos =Barbaros -> Barbaros

espada :: Float -> Objetos
espada peso barbaro = barbaro{fuerza = fuerza barbaro + 2 * peso}


amuletosMisticos  :: String -> Objetos
amuletosMisticos nueva barbaro
    |null (objetos barbaro ) = barbaro {habilidades = nueva : habilidades barbaro }
    |otherwise = barbaro

puerco :: Objetos
puerco = id

marrana :: Objetos
marrana = id



varitasDefectuosas :: Objetos
varitasDefectuosas barbaro = barbaro{habilidades = "magia" : habilidades barbaro, objetos = []}


ardilla :: Objetos
ardilla = id

cuerda :: Objetos -> Objetos -> Objetos
cuerda primerObjero segundoObjero = segundoObjero . primerObjero

dave :: Barbaros
dave = Barbaros {nombre = "Dave", fuerza = 100, habilidades= ["tejer","escribirPoesia"],objetos= ["ardilla"]}

megafono :: Objetos 
megafono barbaro = barbaro {habilidades = [map toUpper . concat $ habilidades barbaro] }
    --                                              "STRINGSTRING"     ["stirng","String"]
    

probarConcat :: [String] -> String
probarConcat listaDeInt = concat listaDeInt

megafonoBarbarico :: Objetos
megafonoBarbarico = cuerda megafono ardilla 

megafonoBarbarico' :: Objetos -> Objetos -> Objetos
megafonoBarbarico' objeto1 objeto2 = megafono . ardilla. cuerda  objeto1 objeto2








