import Data.List

data Leyes = Leyes {
    tema :: Tema,
    presupuesto :: Float,
    partidosPoliticos :: [Partidos]
}deriving(Eq, Show, Ord)

data Partidos = CambioDeTodos | Financiero | CentroFederal | DeportistasAutonomos | PaletaVeloz | DocentesUniversitarios| ClubDeLaPaletaVeloz deriving(Eq,Show, Ord)

data Tema =  Deportes| Educacion | Medicina deriving(Eq, Show, Ord)

leyCannabisMedicinal = Leyes{tema = Medicina, presupuesto = 5, partidosPoliticos = [CambioDeTodos, Financiero]}

leyEducacionSuperior = Leyes{tema = Educacion, presupuesto = 30, partidosPoliticos = [DocentesUniversitarios,CentroFederal]}

leyDeProfesionalizaciónDelTenistaDeMesa = Leyes {tema = Deportes, presupuesto = 1, partidosPoliticos = [CentroFederal, DeportistasAutonomos,ClubDeLaPaletaVeloz]}

leySobreTenis = Leyes{tema = Deportes, presupuesto = 2, partidosPoliticos = [DeportistasAutonomos]}

leyesCompatibles :: Leyes -> Leyes -> Bool
leyesCompatibles primeraLey segundaLey = (tema primeraLey == tema segundaLey) && not (null (partidosPoliticos primeraLey `intersect` partidosPoliticos segundaLey))








