{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use last" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.List

data Ciudadano = Ciudadano {
    profesion :: String,
    sueldoAnal :: Float,
    cantHijos :: Float,
    bienes :: [Bien]
}deriving(Eq, Show, Ord)

type Bien = (String, Float)

type Ciudad = [Ciudadano]

type Pepino = Ciudadano

homero = Ciudadano {profesion = "SeguridadNuclear", sueldoAnal= 20000, cantHijos =3,bienes= [("casa",50000), ("deuda",-70000)]}
frink = Ciudadano {profesion="Profesor", sueldoAnal=12000,cantHijos = 1,bienes=[]}
krabappel = Ciudadano {profesion = "Profesor", sueldoAnal = 1200, cantHijos = 0, bienes = [("casa", 35000)]}
burns = Ciudadano {profesion = "Empresario", sueldoAnal = 300000, cantHijos = 1, bienes = [("empresa",1000000), ("empresa",50000),("auto",200000)]}

springfield :: Ciudad
springfield = [homero,frink,krabappel,burns]


diferenciaDePatrimonio :: Ciudad -> Float
diferenciaDePatrimonio ciudadanos =   diferencia . sort . map calculoPatrimonio $ ciudadanos

diferencia :: [Float] -> Float
diferencia patrimonio = head patrimonio - last  patrimonio
--diferencia' :: [Float] -> Float
--diferencia'  patrimonio =  head patrimonio - head (reverse patrimonio)  

calculoPatrimonio :: Ciudadano -> Float
calculoPatrimonio ciudadano = sueldoAnal ciudadano + (sum . map snd $ bienes ciudadano)

tieneAutoAltaGama :: Ciudadano -> Bool
tieneAutoAltaGama ciudadano = any esAltaGama (bienes ciudadano)

esAltaGama :: Bien -> Bool
esAltaGama ("auto", valor) = valor > 100000

type Medidas =  Ciudadano -> Ciudadano  

auh :: Medidas
auh amiguitaDeNico 
    |calculoPatrimonio amiguitaDeNico == 0 = amiguitaDeNico{sueldoAnal =cantHijos amiguitaDeNico * 1000 } 
    |otherwise = amiguitaDeNico

impuestoGanancias :: Float -> Medidas
impuestoGanancias saldoMinimo ciudadano 
    |sueldoAnal ciudadano > saldoMinimo = ciudadano{sueldoAnal = sueldoAnal ciudadano - sueldoAnal ciudadano * 0.3}
    |otherwise = ciudadano


impuestoAltaGama :: Medidas 
impuestoAltaGama ciudadano 
    |tieneAutoAltaGama ciudadano = ciudadano {sueldoAnal = (sueldoAnal ciudadano -) .  sum . map snd . filter esAltaGama $ bienes ciudadano}
    |otherwise = ciudadano

negociarSueldoProfesion:: Float -> String -> Medidas
negociarSueldoProfesion porcentaje trabajo ciudadano
    |profesion ciudadano == trabajo = ciudadano{sueldoAnal = (sueldoAnal ciudadano +) $ sueldoAnal ciudadano * porcentaje / 100}  


data Gobierno = Gobierno{
    anios :: [Float],
    medidas :: [Medidas]
}--tristelme


gobiernoA = Gobierno { anios= [1999 .. 2003], medidas = [impuestoGanancias 30000 , negociarSueldoProfesion 10 "profesores",
    negociarSueldoProfesion 40 "empresarios", impuestoAltaGama, auh]}

gobiernoB = Gobierno{anios = [2004 .. 2008], medidas = [impuestoGanancias 40000, negociarSueldoProfesion 30 "profesores", negociarSueldoProfesion 40 "camioneros"]}

gobernarUnAnio' :: Gobierno -> Ciudad -> Ciudad 
gobernarUnAnio' gobierno ciudad = foldl (\ciudadanos medidas-> (map medidas) ciudad) ciudad (medidas gobierno) 

gobernarUnAnio :: Gobierno -> Ciudad -> Ciudad 
gobernarUnAnio gobierno ciudad = map (aplicarMedidas (medidas gobierno)) ciudad

aplicarMedidas :: [Medidas] -> Medidas
aplicarMedidas [] ciudadano = ciudadano
aplicarMedidas [x] ciudadano = x ciudadano 
aplicarMedidas (x:xs) ciudadano = aplicarMedidas xs (x ciudadano) 


gobernarPeriodoCompleto :: Gobierno -> Ciudad -> Ciudad
gobernarPeriodoCompleto gobierno ciudad = iterate (gobernarUnAnio gobierno) ciudad !! length (anios gobierno)
-- o con recu 

distribuyóRiqueza :: Gobierno -> Ciudad -> Bool
distribuyóRiqueza gobierno ciudad = (diferenciaDePatrimonio ciudad >) . diferenciaDePatrimonio $ gobernarPeriodoCompleto gobierno ciudad 



-- kaneWest
kaneWest = Ciudadano {profesion = "Empresario",sueldoAnal=100000,cantHijos=0, bienes=infinitosTrineos}

trineos ::Float ->[Bien]
trineos n = ("Rosebud", n) : trineos (n+5)

infinitosTrineos :: [Bien]
infinitosTrineos = trineos 5


-- gobernarUnAnio gobiernoA [kane] = no se va a poder hacer por meterse a la lista infinita de bienes 
-- gobernarUnAnio gobiernoB [kane] = si se va a poder hacer ya que no entra a la lista de bienes y se le aplica impuesto a las ganancias 

-- Dar el tipo más genérico de 
--f1::
-- f1 x y z = map (*x) . filter (y z) 

-- x = Number
-- y = (a -> Bool)
-- z = [a]

--f1 :: Number -> (Number -> Bool) -> [Number] -> [Number]


-- *x