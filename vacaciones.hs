{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Eta reduce" #-}

data Persona = Persona{
    cansancio :: Int,
    estres :: Int,
    idiomas::[String],
    viajaConSuWacha :: Bool
}deriving (Eq, Show, Ord)

type Excursion = Persona -> Persona

data Marea = Fuerte | Moderada | Tranquila deriving(Eq, Show, Ord)

irALaPlaya :: Excursion
irALaPlaya persona
    |viajaConSuWacha persona = persona{cansancio = cansancio persona - 5,estres = estres persona + 3}
    |otherwise = persona{estres = estres persona - 1}

apreciarPaisaje :: String -> Excursion
apreciarPaisaje paisaje persona = persona {estres = estres persona - length paisaje}

--Sabe el Idioma
sabeIdioma :: String -> [String] -> [String]
sabeIdioma idiomaNuevo idiomaQueSe
    |elem idiomaNuevo idiomaQueSe = idiomaQueSe
    |otherwise = idiomaNuevo : idiomaQueSe
              --[idiomaNuevo] ++ idiomaQueSe


hablarOtroIdioma :: String -> Excursion
hablarOtroIdioma idiomaNuevo persona = persona {idiomas = sabeIdioma idiomaNuevo (idiomas persona), viajaConSuWacha = True}

nivelDeIntensidad :: Int
nivelDeIntensidad = 4

caminarMinutos :: Int -> Excursion
caminarMinutos minutos persona = persona {cansancio = cansancio persona - mod minutos nivelDeIntensidad,estres= estres persona - mod minutos nivelDeIntensidad }

paseoEnBarco :: Marea -> Int -> Excursion
paseoEnBarco tipoMarea minutos persona
    |tipoMarea==Fuerte = persona {cansancio = cansancio persona * 10, estres = estres persona * 6 }
    |tipoMarea == Moderada = persona
    |otherwise = hablarOtroIdioma "aleman" . apreciarPaisaje "mar" . caminarMinutos 10 $ persona -- no va minutos, va 10 
                --hablarOtroIdioma "aleman"  (apreciarPaisaje "mar"  (caminarMinutos 10  persona))

fornicarConToto :: Excursion
fornicarConToto suertudo = suertudo{cansancio= 100, estres =0}

ana::Persona
ana = Persona{cansancio = 0, estres = 21, idiomas = ["español"], viajaConSuWacha = True}
cathi::Persona
cathi = Persona{idiomas = ["aleman", "catalan"], viajaConSuWacha = False, cansancio = 15, estres = 15}
beto::Persona
beto = Persona{idiomas = ["aleman"], viajaConSuWacha = False, cansancio = 15, estres = 15}

hacerExcursion :: Persona -> Excursion -> Persona
hacerExcursion persona excurcion = reducirEstres 10 . excurcion $ persona

reducirEstres :: Int -> Excursion
reducirEstres cantReduccion persona = persona{estres = estres persona - estres persona * cantReduccion}

stress :: Persona -> Int
stress = estres

deltaSegun :: (Persona -> Int) ->Persona-> Excursion-> Int
deltaSegun f persona excurcion =  f persona - f (excurcion persona)

--deltaExcursionSegun:: (Persona -> Int) ->Persona-> Excursion-> Int
--deltaExcursionSegun indice persona excursion = deltaSegun indice (excursion persona) persona



--Composicion de Funciones
completo :: Excursion
completo persona = hablarOtroIdioma "melmacquiano" . caminarMinutos 40 . apreciarPaisaje "cascada" . caminarMinutos 20 $ persona

ladoB :: Int -> Excursion -> Excursion
ladoB duracion excursion = caminarMinutos 120 . excursion . paseoEnBarco Tranquila duracion 

excursionDeIsla :: [Excursion] -> Excursion
excursionDeIsla excursion persona = foldr ( \persona excursion-> persona excursion) persona excursion

islaVecina:: Marea -> Int -> [Excursion] -> Excursion
islaVecina marea tiempoDeViaje excursion =  paseoEnBarco marea tiempoDeViaje .excursionDeIsla excursion. paseoEnBarco marea tiempoDeViaje 

isla :: Marea-> Int -> Excursion
isla marea numero persona
    |marea == Fuerte = islaVecina marea numero [apreciarPaisaje "lago"] persona
    |otherwise = islaVecina marea numero [irALaPlaya] persona  




-- te gustan mas ♂? o ♀? 
--Respuesta: