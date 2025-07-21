data Persona = Persona{
    edad :: Float,
    peso :: Float,
    tonificacion :: Int
} deriving (Eq, Show, Ord)

pancho = Persona 40 120 1

andres = Persona 22 80 6

estaSaludable :: Persona -> Bool
estaSaludable persona = tonificacion persona > 5 && not (estaObeso persona) 

estaObeso :: Persona -> Bool
estaObeso persona = peso persona > 100

caloriasPorKilo :: Float
caloriasPorKilo = 150

bajarDePeso :: Persona -> Float -> Persona
bajarDePeso persona calorias 
    |estaObeso persona = persona {peso = peso persona - (calorias / caloriasPorKilo )}
    |not (estaObeso persona) && edad persona > 30 && calorias > 200 = persona{peso = peso persona - 1}
    |otherwise = persona{peso=peso persona - (calorias / (peso persona * edad persona))}