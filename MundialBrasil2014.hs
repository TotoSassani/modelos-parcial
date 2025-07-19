data Jugador = Jugador{
    nombreJugador :: String,
    edad :: Float,
    promedioGol :: Float,
    habilidad :: Float,
    valorCansancio :: Float
} deriving (Show, Eq)

data Equipo = Equipo {
    nombreEquipo :: String,
    grupoAlQuePertenece :: Char,
    jugadoresDelEquipo :: [Jugador]
} deriving (Eq,Show)



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
