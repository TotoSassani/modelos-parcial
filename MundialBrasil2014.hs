{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move filter" #-}
import Data.Type.Equality (TestEquality)
{-# HLINT ignore "Eta reduce" #-}

data Jugador = Jugador{
    nombreJugador :: String,
    edad :: Float,
    promedioGol :: Float,
    habilidad :: Float,
    valorCansancio :: Float
} deriving (Show, Eq,Ord)

data Equipo = Equipo {
    nombreEquipo :: String,
    grupoAlQuePertenece :: Char,
    jugadoresDelEquipo :: [Jugador]
} deriving (Eq,Show,Ord)



martin = Jugador "Martin" 26 0 50 35
juan = Jugador "Juancho" 30 0.2 50 40
maxi = Jugador "Maxi Lopez" 27 0.4 68 30
jonathan = Jugador "Chueco" 20 1.5 80 99
lean = Jugador "Hacha" 23 0.01 50 35
brian = Jugador "Panadero" 21 5 80 15
garcia = Jugador "Sargento" 30 1 80 13
messi = Jugador "Pulga" 26 10 99 43
aguero = Jugador "Aguero" 24 5 90 5

equipo1 = Equipo "Lo Que Vale Es El Intento" 'F' [martin, juan, maxi]
losDeSiempre = Equipo "Los De Siempre" 'F' [jonathan, lean, brian]
restoDelMundo = Equipo "Resto Del Mundo" 'A' [garcia, messi, aguero]

--1)
figura :: Equipo -> [Jugador]
figura equipo = filter ((>75). habilidad). filter ((> 0) . promedioGol) $ jugadoresDelEquipo equipo

habilidadJugador :: Equipo -> [Jugador]
habilidadJugador equipo = filter ((> 75) . habilidad) (jugadoresDelEquipo equipo)

promedioGolJugador :: [Jugador]-> [Jugador]
promedioGolJugador  = filter ((> 0) . promedioGol)

figura' :: Equipo -> [Jugador]
figura' equipo =  promedioGolJugador . habilidadJugador $ equipo

jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

tieneFarandulero :: Equipo -> Bool
tieneFarandulero equipo = any ((`elem` jugadoresFaranduleros) . nombreJugador) (jugadoresDelEquipo equipo)

tieneFarandulero' :: Jugador -> Bool
tieneFarandulero' = (`elem` jugadoresFaranduleros) . nombreJugador

-- 3)
figuritaDificil :: Char -> [Equipo] -> [Jugador]
figuritaDificil letra equipo = filter (not . tieneFarandulero')  . filter ((< 27) . edad). concatMap figura . filter ((== letra). grupoAlQuePertenece) $ equipo

esJugadorJoven :: Jugador -> Bool
esJugadorJoven jugador = edad jugador < 27

esFigurita ::Equipo -> Bool
esFigurita equipo = null (figura equipo)

