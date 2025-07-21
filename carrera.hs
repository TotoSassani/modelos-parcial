data Auto = Auto{
    color :: String, 
    velocidad :: Int,
    distanciaRecorrida :: Int
} deriving (Eq, Show, Ord)

type Carrea = [Auto]

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = auto1 /= auto2 && distanciaEntreAutos auto1 auto2 < 10

distanciaEntreAutos :: Auto -> Auto -> Int
distanciaEntreAutos auto1  = abs . (distanciaRecorrida auto1 -) . distanciaRecorrida 


