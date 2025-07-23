{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.Char ( isUpper, toUpper )
import Distribution.Compat.Prelude (isDigit)
{-# HLINT ignore "Use newtype instead of data" #-}

data Tesoro = Tesoro{
    anioDescubrimiento :: Float,
    precio :: Float,
    claveMorce :: Cerradura
}deriving (Eq,Show)

data Cerradura = Cerradura {
    claveCompuesta :: String
}deriving (Eq,Show)


-- Parte 1
tesoroLujoso :: Tesoro -> Bool
tesoroLujoso tesoro = anioDescubrimiento tesoro >= 200 || precio tesoro > 1000
-- tesoroLujoso tesoro = anioDescubrimiento tesoro >= 200 
-- tesoroLujoso tesoro = precio tesoro > 1000

tesoroDeTela :: Tesoro -> Bool
tesoroDeTela tesoro = precio tesoro < 50 && not (tesoroLujoso tesoro)

tesoroEstandar :: Tesoro -> Bool
tesoroEstandar tesoro = not (tesoroLujoso tesoro) && not (tesoroDeTela tesoro)

valorDeTesoro :: Tesoro -> Float
valorDeTesoro tesoro = precio tesoro + 2 * anioDescubrimiento tesoro

-- Parte 2

cerraduraAbierta :: Cerradura -> Bool
cerraduraAbierta cerradura = null (claveCompuesta cerradura)

type Herramienta = Cerradura -> Cerradura 

herramientaMartillo :: Herramienta
herramientaMartillo cerradura = cerradura {claveCompuesta = drop 3 (claveCompuesta cerradura)}

laLlaveMaestra :: Herramienta
laLlaveMaestra cerradura = cerradura {claveCompuesta = []}

lasGanzuas :: (Char->Bool)-> Herramienta
lasGanzuas eliminarCaracter cerradura = cerradura {claveCompuesta =  filter (not . eliminarCaracter)
    $ drop 1 (claveCompuesta cerradura)}

laGanzuaGancho :: Herramienta
laGanzuaGancho cerradura = lasGanzuas isUpper cerradura

laGanzuaRastrillo :: Herramienta
laGanzuaRastrillo laMauskiHerramientaDeNico = lasGanzuas isDigit laMauskiHerramientaDeNico

laGanzuaRombo :: String -> Herramienta
laGanzuaRombo inscripcion cerradura = lasGanzuas (`elem` inscripcion) cerradura

pruebaRombo = Cerradura {claveCompuesta= "El abecedario"}

elTensor :: Herramienta
elTensor cerradura = cerradura {claveCompuesta = map toUpper (claveCompuesta cerradura)}

elSocotroco :: Herramienta -> Herramienta -> Herramienta
elSocotroco primeraHerramienta segundaHerramienta =  segundaHerramienta . primeraHerramienta

-- Parte 3
data Ladron = Ladron{
    nombre :: String,
    herramientas :: [Herramienta],
    tesorosRobados :: [Tesoro]
}

experiencia :: [Tesoro] -> Float
experiencia tesoro = sum (map precio tesoro)

ladronPremium :: Ladron -> Bool
ladronPremium ladron = experiencia (tesorosRobados ladron) > 100 && all tesoroLujoso (tesorosRobados ladron)

robarCofre :: Ladron-> Tesoro -> Ladron
robarCofre ladron cofre
    |cerraduraAbierta (claveMorce cofre)  = ladron {tesorosRobados = cofre : tesorosRobados ladron }
    |null (herramientas ladron) = ladron
    |otherwise = robarCofre (eliminarHerramientas ladron) (utlizarHerramientas (herramientas ladron) cofre )

eliminarHerramientas :: Ladron -> Ladron
eliminarHerramientas ladron = ladron{herramientas = drop 1 (herramientas ladron)}

utlizarHerramientas :: [Herramienta] -> Tesoro -> Tesoro
utlizarHerramientas herramientas cofre = cofre{ claveMorce = head herramientas (claveMorce cofre) }

atracoLlevado :: [Tesoro]->Ladron -> Ladron
atracoLlevado cofres ladron = foldl robarCofre ladron cofres

-- inverse . foldl robarCofres ladron . inverse $ cofres == foldr
-- inverse . foldr robarCofres ladron . inverse $ cofres == foldl

--Infinitas herramientas y cofres
herramientasInfinitasMartillo :: [Herramienta]
herramientasInfinitasMartillo = repeat herramientaMartillo

ladronSuperHabilidoso =Ladron{
    nombre = "Toto",
    herramientas = herramientasInfinitasMartillo,
    tesorosRobados =[]
    }

cerraduraInfinita :: Cerradura
cerraduraInfinita = Cerradura (cycle "XYZ")

cofreImposibleDeAbrirMuyCapo = Tesoro{
    anioDescubrimiento =200,
    precio = 0,
    claveMorce = cerraduraInfinita
}

--LISTA inf de tesoros del ladron


