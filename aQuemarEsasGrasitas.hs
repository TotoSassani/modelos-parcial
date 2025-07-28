data Persona = Persona{
    edad :: Float,
    peso :: Float,
    tonificacion :: Float
} deriving (Eq, Show, Ord)

type Rutina = Persona -> Persona

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

caminata ::  Float -> Float -> Rutina
caminata _ _ presona = presona{peso = peso presona - 5}

entrenamientoCinta :: Float->Float -> Rutina
entrenamientoCinta _ minutos persona = persona{peso = peso persona-6 }-- mod minutos 5}

pesas :: Float -> Float -> Rutina
pesas pesoALevantar tiempo persona
    |tiempo > 10 = persona{ tonificacion = tonificacion persona + pesoALevantar * 0.1}
    |otherwise = persona

colina :: Float->Float -> Rutina
colina inclinacion minutos persona = persona{ peso = peso persona - 2 * minutos * inclinacion}

montania :: Float-> Float -> Rutina
montania inclinacion minutos = tonificar . colina (3*inclinacion) (minutos / 2) . colina inclinacion (minutos / 2)

tonificar :: Rutina
tonificar persona = persona{tonificacion = tonificacion persona + 1}


-- hacerEjercicoConRecu :: Persona-> Float->Float ->[Rutina] -> Persona
-- hacerEjercicoConRecu persona _ _ [] = persona
-- hacerEjercicoConRecu persona resto minutos [ejercicio] = ejercicio resto minutos persona
-- hacerEjercicoConRecu persona resto minutos (x:xs:xss) = x resto minutos persona : hacerEjercicoConRecu (xs: xss) resto minutos persona

-- hacerEjercicoConFold :: [Rutina] -> Persona-> Float-> Float ->Persona
-- hacerEjercicoConFold ejercicios persona resto minutos  = foldl (\p rutina -> rutina resto minutos p) persona ejercicios

restarKilos :: Persona -> Persona -> Float
restarKilos personaDespues personaAntes = peso personaAntes - peso personaDespues

-- kilosPerdidos :: Persona ->Float->Float ->[Rutina] -> Float
-- kilosPerdidos persona resto minutos ejercicios = restarKilos (hacerEjercicoConRecu persona resto minutos ejercicios) persona

-- tonificacionResta :: Persona -> Persona -> Float
-- tonificacionResta personaDespues personaAntes = tonificacion personaDespues - tonificacion personaAntes

-- tonificacionGanado ::Persona ->Float->Float ->[Rutina] -> Float
-- tonificacionGanado persona resto minutos ejercicios = tonificacionResta (hacerEjercicoConRecu persona resto minutos ejercicios) persona

--Loma Montero















