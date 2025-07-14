{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.List (isPrefixOf, head, sortBy)
import GHCi.Message (THResultType)


data Jugador = Jugador {
    nombre :: String,
    velocidad :: Float,
    habilidad:: Float,
    puestoDondeJuega :: Posicion,
    partidos:: [Partidos]
}deriving (Eq, Show, Ord)

data Partidos = Partido{
    goles :: Int,
    minutos::Int
}deriving (Eq, Show, Ord)

data Posicion = Arquero | Defensor | Volante | Delantero deriving (Eq, Show, Ord)

type Equipo = [Jugador]
-- ==================================Tema1======================================

jugadorToto = Jugador { nombre = "Toto",velocidad=45,habilidad=100, puestoDondeJuega = Delantero,
    partidos = [Partido {goles = 45,minutos = 90},Partido {goles = 46,minutos = 48}]}


jugadorTino :: Jugador
jugadorTino = Jugador { nombre = "Santino",velocidad=2000,habilidad=50, puestoDondeJuega = Delantero,
    partidos = [Partido {goles = 5,minutos = 48},Partido {goles = 0,minutos = 45}]}

-- por todos los partidos una cant de min
nombreEquipo :: Int ->Equipo->String -> [String]
nombreEquipo cantMinutos equipo letra  = filter (isPrefixOf letra ) .map nombre .
    filter ( all ((>=cantMinutos). minutos) . partidos) $ equipo


--suma de todos los partidos a una cant de min
nombreEquipo1 :: Int ->Equipo->String->[String]
nombreEquipo1 cantMinutos equipo letra =  filter (isPrefixOf letra ) . map nombre . filter ((>=cantMinutos). sum .map minutos . partidos) $ equipo


type Tecnico = Jugador->Jugador

tacticaBielsa:: Jugador->Jugador
tacticaBielsa jugador = jugador { velocidad = velocidad jugador * 1.5, habilidad = habilidad jugador - 10}

-- bielsa' :: Tecnico
-- bielsa' = \jugador -> jugador{velocidad = velocidad jugador * 1.5, habilidad = habilidad jugador - 10}

bielsa = tacticaBielsa

-- menotti' ::Int -> Tecnico
-- menotti' numbr = \jugador -> jugador {nombre="Mr. "++nombre jugador , habilidad=habilidad jugador + numbr}

tacticaMenotti :: Float -> Jugador -> Jugador
tacticaMenotti puntosHabilidad jugador = jugador { nombre = "Mr. " ++ nombre jugador, 
    habilidad = habilidad jugador + puntosHabilidad}

menotti :: Float -> Tecnico
menotti number = tacticaMenotti number

-- bertolotti' :: Tecnico
-- bertolotti' = menotti' 10

bertolotti :: Tecnico
bertolotti = tacticaMenotti 10

vanGaal :: Tecnico
vanGaal = id

-- vanGaal(menotti numro(bielsa jugador)) 

jugadorBueno :: Jugador -> Bool
jugadorBueno jugador = habilidad jugador >= velocidad jugador
jugadorBueno jugador = puestoDondeJuega jugador == Volante
-- jugadorBueno jugador = habilidad jugador >= velocidad jugador ||  puestoDondeJuega jugador == Volante

mejora :: Tecnico -> Equipo-> Bool
mejora tecnico equipo = length (filter jugadorBueno (map tecnico equipo)) > length (filter jugadorBueno equipo)

esImparable :: Jugador -> Bool
esImparable jugador = goleadorEnRacha (partidos jugador)


goleadorEnRacha :: [Partidos]->Bool
goleadorEnRacha [] = False
goleadorEnRacha [_]=True
goleadorEnRacha (x:xs:xss)--[1.2.3.3.5]
    |goles x < goles xs = goleadorEnRacha(xs:xss)
    |otherwise = False  

-- ===============================TEMA2========================================

-- marcaronGolesEnTodosLosPartidos :: Equipo -> [String]
-- marcaronGolesEnTodosLosPartidos equipoLince = map nombre . filter(all((> 0) . goles). partidos) $ equipoLince

marcaronGolesEnTodosLosPartidos :: Equipo -> Int
marcaronGolesEnTodosLosPartidos equipoLince = length . filter(all((> 0) . goles). partidos) $ equipoLince