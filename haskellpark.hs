{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use null" #-}

data Atraccion = Atraccion {
    nombre :: String,
    alturaRequerida :: Int,
    duracion :: Int,
    opiniones :: [String],
    estaEnMantenimiento :: [Mantenimiento]
}

data Mantenimiento = Mantenimiento {
    duracionMantenimiento :: Int,
    trabajoARealizar :: String
}

atraccionNico = Atraccion { nombre= "hola", alturaRequerida = 2, duracion = 11, opiniones = ["veloz"],
    estaEnMantenimiento = [Mantenimiento{duracionMantenimiento = 10, trabajoARealizar = "Arreglo"}]}


--Si es por casos separados:
masBuenaQue :: Atraccion -> Int
masBuenaQue atraccion
    |duracion atraccion > 10 = 100
    |otherwise = cantidadDeReparaciones atraccion

cantidadDeReparaciones :: Atraccion -> Int
cantidadDeReparaciones atraccion
    |length (estaEnMantenimiento atraccion) < 3 = 10 * length (nombre atraccion) + 2 * length (opiniones atraccion)
    |otherwise = 10 * alturaRequerida atraccion

type Reparacion = Atraccion -> Atraccion

ajusteDeTornillería :: Int -> Reparacion
ajusteDeTornillería tornillos atraccion
    | duracion atraccion + tornillos <= 10 = atraccion{duracion=10, estaEnMantenimiento = tail (estaEnMantenimiento atraccion)}
    | otherwise = atraccion{duracion=10}

engrase :: Int -> Reparacion
engrase cantGrasa atraccion = atraccion {alturaRequerida = alturaRequerida atraccion + 1 * cantGrasa, opiniones = ["para valientes"] ++ opiniones atraccion, estaEnMantenimiento = tail (estaEnMantenimiento atraccion) } -- es 0.1 pero no nos funciona entonces ponemos 1

meQuedoConLasDos:: [String]->[String]
meQuedoConLasDos (x:xs:xss) = [x,xs]

mantenimientoElectrico :: Reparacion
mantenimientoElectrico atraccion = atraccion {opiniones = meQuedoConLasDos (opiniones atraccion), estaEnMantenimiento = tail (estaEnMantenimiento atraccion)}

mantenimientoBasico :: Reparacion
mantenimientoBasico = ajusteDeTornillería 8 . engrase 10

meDaMiedito :: Atraccion -> Bool
meDaMiedito  atraccion = any ((>= 4) . duracionMantenimiento) . estaEnMantenimiento $ atraccion --any aplica (>= 4) a cada elemento de la lista
--meDaMiedito :: Atraccion -> Bool
--meDaMiedito  atraccion = any ((>= 4) . duracionMantenimiento) (estaEnMantenimiento atraccion)

acaCerramos :: Atraccion ->Bool
acaCerramos atraco = (>= 7). sum . map duracionMantenimiento $ (estaEnMantenimiento atraco) --sum no aplica nada a la lista por eso se usa map

quilmesEsMasGrandeQueDisney ::[Atraccion] ->Bool
quilmesEsMasGrandeQueDisney atraccion = not(any ((>=1). length . estaEnMantenimiento) atraccion) && all ((>5). length . nombre) atraccion
--quilmesEsMasGrandeQueDisney atraccion = all (null . estaEnMantenimiento) atraccion && all ((>5). length . nombre) atraccion 
