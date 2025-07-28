{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}

import Data.ByteString ( isPrefixOf )
import Data.Char

{-# HLINT ignore "Use infix" #-}
data Plomero = Plomero{
    nombre :: String,
    herramientas :: [Herramienta],
    historialReparaciones :: [String],
    plata :: Int
}deriving (Eq, Show,Ord)

data Herramienta = Herramienta{
    nombreHerramienta :: String,
    precio :: Int,
    materialEmpuniadura :: Material
}deriving (Eq, Show,Ord)

data Material = Hierro | Madera | Goma | Plastico deriving (Eq, Show,Ord)

llaveInglesaDeHierro = Herramienta{nombreHerramienta ="llaveInglesa", precio = 2000,materialEmpuniadura = Hierro}

martilloDeMadera = Herramienta{nombreHerramienta ="martillo", precio = 200 , materialEmpuniadura = Madera}

plomeroMario = Plomero "Mario " [llaveInglesaDeHierro, martilloDeMadera] [] 12000

plomeroWario = Plomero "Wario" infinitasLlavesFrancesas [] 5

infinitasLlavesFrancesas :: [Herramienta]
infinitasLlavesFrancesas = generarLlavesDesde 0

generarLlavesDesde :: Int -> [Herramienta]
generarLlavesDesde n = Herramienta "llaveFrancesa" n Hierro : generarLlavesDesde (n + 1)

tieneHerramienta ::  String ->Plomero-> Bool
tieneHerramienta herramienta plomero  =  herramienta `elem` map nombreHerramienta (herramientas plomero)

esMalvado :: Plomero -> Bool
esMalvado plomero = "Wa" == take 2 (nombre plomero)
-- esMalvado' :: Plomero -> Bool
-- esMalvado' plomero = isPrefixOf "Wa" (nombre plomero)

puedeComprar :: Plomero -> Herramienta -> Bool
puedeComprar plomero herramienta = plata plomero >= precio herramienta 


esBuenaHerramienta :: Herramienta -> Bool
esBuenaHerramienta herramienta = materialEmpuniadura herramienta == Hierro && precio herramienta > 10000
esBuenaHerramienta herramienta = nombreHerramienta herramienta == "martillo" && 
    ( materialEmpuniadura herramienta ==Madera || materialEmpuniadura herramienta==Goma)


comprarHerramienta :: Plomero-> Herramienta -> Plomero
comprarHerramienta plomero herramienta 
    |puedeComprar plomero herramienta = plomero{herramientas =herramienta : herramientas plomero }
    |otherwise = plomero

data Reparacion = Reparacion {
    descripcion :: String,
    requerimiento :: Plomero -> Bool
}  

filtraciónDeAgua = Reparacion {descripcion = "filtración de agua", requerimiento = tieneHerramienta "llaveInglesa" }

reparacionDificil :: Reparacion -> Bool
reparacionDificil reparacion = length (descripcion reparacion) > 100 && all (isUpper) (descripcion reparacion)

costeDeReparacion :: Reparacion -> Int
costeDeReparacion reparacion = length (descripcion reparacion) * 3  

puedeHacerReparacion :: Reparacion -> Plomero -> Bool
puedeHacerReparacion reparacion plomero = (requerimiento reparacion) plomero
puedeHacerReparacion reparacion plomero = esMalvado plomero && tieneHerramienta "martillo" plomero

herramientaRobada :: Herramienta    
herramientaRobada = Herramienta {nombreHerramienta = "destornillador" , precio = 0 , materialEmpuniadura = Plastico}

hacerReparacion  :: Reparacion -> Plomero -> Plomero
hacerReparacion reparacion plomero 
    |puedeHacerReparacion reparacion plomero && esMalvado plomero= plomero {herramientas = herramientaRobada : herramientas plomero, 
            plata = plata plomero + costeDeReparacion reparacion}

    |puedeHacerReparacion reparacion plomero && reparacionDificil reparacion =plomero {herramientas = filter(not.esBuenaHerramienta) (herramientas plomero)  , 
            plata = plata plomero + costeDeReparacion reparacion} 

    |puedeHacerReparacion reparacion plomero = plomero {herramientas = drop 1 (herramientas plomero), 
            plata = plata plomero + costeDeReparacion reparacion} 

    | otherwise = plomero {plata = plata plomero + 100}


jornadaDeTrabajo ::[Reparacion] -> Plomero -> Plomero
jornadaDeTrabajo reparacion plomero  = foldl (\plomero reparacion ->hacerReparacion reparacion plomero ) plomero reparacion

hacerTrabajar :: [Plomero] -> [Reparacion] -> [Plomero]
hacerTrabajar plomero reparaciones = map (jornadaDeTrabajo reparaciones) plomero


--elEmpleadoMasReparado :: [Reparacion]->[Plomero]  -> Plomero
--elEmpleadoMasReparado reparacion [x] =  x 
--elEmpleadoMasReparado reparacion (plomero:plomerin: plomeros)
--    |length(historialReparaciones plomero) > length(historialReparaciones plomerin) && length(historialReparaciones plomero)> length(historialReparaciones (elEmpleadoMasReparado reparacion plomeros)) = hacerTrabajar plomero reparacion  
--    |otherwise = elEmpleadoMasReparado reparacion (plomerin: plomeros) 


elEmpleadoMasReparado' :: [Plomero] -> [Reparacion] -> Plomero
elEmpleadoMasReparado' plomeros reparaciones =
  foldl1 maxPorReparaciones (hacerTrabajar plomeros reparaciones)

maxPorReparaciones :: Plomero -> Plomero -> Plomero
maxPorReparaciones p1 p2
  | length (historialReparaciones p1) >= length (historialReparaciones p2) = p1
  | otherwise = p2

masAdinerado :: [Plomero] -> [Reparacion] -> Plomero
masAdinerado plomeros reparaciones = foldl1 maxPorPlata (hacerTrabajar plomeros reparaciones)

maxPorPlata :: Plomero -> Plomero -> Plomero
maxPorPlata p1 p2
  |plata p1 > plata p2 = p1
  |otherwise = p2

masInvirtio :: [Plomero] -> [Reparacion] -> Plomero
masInvirtio plomeros reparaciones = foldl1 maxPorHerramientas (hacerTrabajar plomeros reparaciones)

maxPorHerramientas :: Plomero -> Plomero -> Plomero
maxPorHerramientas p1 p2
  |(sum . map precio . herramientas) p1 > (sum . map precio . herramientas) p2 = p1 
  |otherwise = p2