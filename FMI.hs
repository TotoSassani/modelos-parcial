{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import System.Win32 (SYSTEM_INFO(siMaximumApplicationAddress))
{-# HLINT ignore "Eta reduce" #-}
data Pais = Pais{
    ingrespPerCapita :: Float,
    poblacionActivaSectorPublico :: Float,
    poblacionActivaSectorPivado :: Float,
    recursos :: [String],
    deuda :: Float
} deriving (Eq, Show, Ord)

type Fmi = Pais -> Pais

paisClamidia = Pais{ingrespPerCapita = 4140, poblacionActivaSectorPublico = 400000, poblacionActivaSectorPivado = 650000, recursos = ["Mineria","Ecoturismo","Petroleo"],
     deuda = 50}

--recetas 1
prestarMillones :: Float -> Fmi
prestarMillones millones pais = pais{deuda = deuda pais + millones * 1.5}

--recetas 2
reducirPuestoDeTrabajoPublico :: Float -> Fmi
reducirPuestoDeTrabajoPublico numero pais = pais{poblacionActivaSectorPublico = poblacionActivaSectorPivado pais - numero,
    ingrespPerCapita = ingresoFmi pais}

--receta 3
--Si es que se le saca todos los recursos naturales
darleEmpresaAlFmi :: Fmi
darleEmpresaAlFmi pais = pais{deuda = deuda pais - deudaADescontar, recursos = []}
--Si solo se le saca el Recurso natural que piden
darleEmpresaAlFmi' :: String -> Fmi
darleEmpresaAlFmi' natural pais = pais{deuda = deuda pais - deudaADescontar, recursos = filter (/=natural) (recursos pais) }

--recetas 4
blindaje :: Fmi
blindaje pais = pais {deuda = deuda pais + pbi pais, poblacionActivaSectorPublico = disminuirPoblacionJudia personasADisminuirParaBlindaje pais}

deudaADescontar :: Float
deudaADescontar = 2 -- en millones de dolares

ingresoFmi:: Pais-> Float
ingresoFmi pais
    |poblacionActivaSectorPublico pais >= 100 = ingrespPerCapita pais - ingrespPerCapita pais * 0.2
    |otherwise = ingrespPerCapita pais - ingrespPerCapita pais * 0.15



disminuirPoblacionJudia :: Float -> Pais ->Float
disminuirPoblacionJudia cantDePersonas pais
    |poblacionActivaSectorPublico pais < cantDePersonas = 0
    |otherwise = poblacionActivaSectorPublico pais - cantDePersonas

personasADisminuirParaBlindaje :: Float
personasADisminuirParaBlindaje = 500

pbi :: Pais -> Float --ingresopercapita * (suma puestospublicos y privados)
pbi persona = ingrespPerCapita persona * (poblacionActivaSectorPublico persona + poblacionActivaSectorPivado persona)

prestarPlata :: Float
prestarPlata = 200

recursoPrestado :: String
recursoPrestado = "Mineria"

prestamoFmi :: Fmi
prestamoFmi pais = darleEmpresaAlFmi . prestarMillones prestarPlata $ pais
-- prestamoFmi  = darleEmpresaAlFmi  . prestarMillones prestarPlata

prestamoFmi' :: Fmi
prestamoFmi' = darleEmpresaAlFmi' recursoPrestado . prestarMillones prestarPlata 

darPrestamo :: Pais
darPrestamo = prestamoFmi paisClamidia

darPrestamo' :: Pais
darPrestamo' = prestamoFmi' paisClamidia

puedeZafar :: [Pais]-> [Pais]
puedeZafar = filter (all (== "Petroleo") . recursos)

deudaTotal :: [Pais] -> Float
deudaTotal paisesDeudores = sum (map deuda paisesDeudores)

--Orden superior en cuando uno se mete adentro del otro, es decir toma el argumento anterior
--composición y aplicación parcialLa composición se refiere a la combinación de funciones para aplicar
--una después de la otra, mientras que la aplicación parcial implica proporcionar menos argumentos de los 
--esperados a una función, lo que resulta en una nueva función que acepta los argumentos restantes

ordenadaDePeor :: Pais ->[Fmi] ->Bool
ordenadaDePeor _ [] = True
ordenadaDePeor _ [_] = True
ordenadaDePeor pais (x:xs:xss) = aplicarReceta x pais  <=  aplicarReceta xs (x pais) && ordenadaDePeor (x pais) (xs: xss)


aplicarReceta :: Fmi -> Pais ->Float
aplicarReceta receta pais  = pbi  (receta pais)

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos


paisNirvana = Pais{ingrespPerCapita = 4140, poblacionActivaSectorPublico = 400000, poblacionActivaSectorPivado = 650000, recursos = recursosNaturalesInfinitos,
     deuda = 50}

