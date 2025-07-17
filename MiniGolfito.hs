{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import GHCi.Message (THResultType)
import System.Win32 (fILE_FLAG_SEQUENTIAL_SCAN)

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Float
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Float,
  altura :: Float
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Habilidad -> Tiro

putter :: Float -> Palo
putter _ habilidad = UnTiro 
    {velocidad = 10,
    precision = precisionJugador habilidad * 2, 
    altura = 0
    }

madera :: Float ->Palo
madera _ habilidad = UnTiro 
    {velocidad = 100,
    altura = 5, 
    precision = precisionJugador habilidad / 2
    }


alturaPegada :: Float -> Float-> Float
alturaPegada pegada resta
    |pegada<3 = 0
    |otherwise = pegada - resta

hierro :: Float -> Palo
hierro  n habilidad = 
    UnTiro {
    velocidad = fuerzaJugador  habilidad,
    precision = precisionJugador habilidad / n, 
    altura = alturaPegada n 3
}

type Palos = [Palo]

golpe :: Jugador -> Palo -> Tiro
golpe jugador palos = palos (habilidad jugador)

tiroNoSuperado :: Tiro
tiroNoSuperado = UnTiro{precision=0,velocidad=0,altura=0}

tunelConRampita :: Float -> Tiro -> Tiro
tunelConRampita _ unTiro 
    | precision unTiro >= 90 && altura unTiro == 0 = unTiro {precision = 100, velocidad = velocidad unTiro *2}
    | otherwise = tiroNoSuperado 

lagunaSuperada :: Float -> Tiro -> Tiro
lagunaSuperada largoLaguna unTiro 
    |velocidad unTiro > 80 && altura unTiro >= 1 && altura unTiro <= 5 = 
        unTiro {altura = altura unTiro/largoLaguna}
    | otherwise = tiroNoSuperado 

hoyoAgrandado:: Float -> Tiro -> Tiro
hoyoAgrandado _ tiro
    |velocidad tiro >=5 && velocidad tiro <= 20 && altura tiro == 0 && precision tiro > 95 = tiroNoSuperado
    |otherwise = tiroNoSuperado 

data Obstaculos = TunelConRampita | LagunaSuperada | HoyoAgrandado deriving(Eq, Show)

superaConExito :: Jugador -> Obstaculos -> Float -> Palo -> Bool
superaConExito jugador obstaculo dificultad palo = superaObstaculo obstaculo dificultad (golpe jugador palo) /= tiroNoSuperado

palosUtiles :: Jugador -> Obstaculos -> Float -> [Palo] -> [Palo]
palosUtiles jugador obstaculo dificultad palos =
  filter (superaConExito jugador obstaculo dificultad) palos

superaObstaculo :: Obstaculos -> Float -> Tiro -> Tiro
superaObstaculo TunelConRampita dificultad = tunelConRampita dificultad
superaObstaculo LagunaSuperada dificultad = lagunaSuperada dificultad
superaObstaculo HoyoAgrandado dificultad = hoyoAgrandado dificultad




































































