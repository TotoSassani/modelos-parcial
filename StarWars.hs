{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}

data Nave = Nave {
    nombre :: String,
    durabilidad :: Float,
    escudo :: Float,
    ataque :: Float,
    poderEspecial :: Poder 
}

type Poder =  Nave -> Nave 

type Flota = [Nave]

naveTIEFighter = Nave "TIE Fighter" 200 100 50 movimientoTurbo

naveXWing = Nave "X Wing" 300 150 100 reparacionEmergencia

naveNaveDeDarthVader = Nave "Nave de Darth Vader" 500 300 200 superTurbo

naveMillenniumFalcon = Nave "Millennium Falcon" 1000 500 50 escudoNave

naveAguanteQuilmes = Nave "Aguante Quilmes" 80 1000 50 habilidadNaveAguanteQuilmes

movimientoTurbo :: Poder
movimientoTurbo = \nave -> nave {ataque = ataque nave + 25} 

superTurbo :: Poder 
superTurbo nave = bajarDurabilidad 45 . movimientoTurbo . movimientoTurbo $ nave

bajarDurabilidad :: Float -> Poder
bajarDurabilidad disminucion nave = nave {durabilidad = durabilidad nave - disminucion}

escudoNave :: Poder  
escudoNave nave = incrementarEscudo 100 . reparacionEmergencia $ nave

incrementarEscudo :: Float -> Poder
incrementarEscudo n nave = nave {escudo = escudo nave + n}

reparacionEmergencia :: Poder 
reparacionEmergencia nave = nave{durabilidad = durabilidad nave + 50, ataque = ataque nave - 30}

habilidadNaveAguanteQuilmes :: Poder
habilidadNaveAguanteQuilmes nave =  superTurbo . escudoNave $ nave 

durabilidadFlota :: Flota -> Float
durabilidadFlota listaNaves = sum (map durabilidad listaNaves) -- sum . map durabilidad 


atacar :: Nave -> Nave -> Nave
atacar atacante atacado 
    |escudo atacado < ataque atacante = atacado {durabilidad = escudo (poderEspecial atacado atacado) - ataque (poderEspecial atacante  atacante) }
    |otherwise = atacado

type Estrategia = Nave -> Bool

fueraDeCombate ::Estrategia
fueraDeCombate nave = durabilidad nave == 0

navesDebiles :: Estrategia
navesDebiles nave = escudo nave < 200

navesConPeligrosidad :: Float -> Estrategia
navesConPeligrosidad n nave = ataque nave > n 

navesFueraDeCombate :: Estrategia 
navesFueraDeCombate nave = fueraDeCombate nave

navesFuertes :: Estrategia 
navesFuertes nave = escudo nave >= 400

misiónSorpresa :: Nave -> Flota -> Estrategia -> Flota
misiónSorpresa nave flotaEnemiga estrategia = map (atacar nave). filter estrategia $ flotaEnemiga

minimizaDurabilidad :: Nave -> Flota -> Estrategia -> Estrategia -> Flota
minimizaDurabilidad nave navesEnemigas estrategia1 estrategia2 
    |durabilidadFlota  (misiónSorpresa nave navesEnemigas estrategia1) < durabilidadFlota (misiónSorpresa nave navesEnemigas estrategia2) = misiónSorpresa nave navesEnemigas estrategia1
    |otherwise = misiónSorpresa nave navesEnemigas estrategia2

-- misiónSorpresa :: Nave -> Flota -> [Estrategia] -> Flota
-- misiónSorpresa nave flotaEnemiga estrategia = map (atacar nave) (filter (\nave -> all (\estrategia-> estrategia nave)estrategia) flotaEnemiga)

flotaInfinita :: Flota
flotaInfinita = cycle[naveTIEFighter,naveXWing,naveNaveDeDarthVader]

-- ¿Es posible determinar su durabilidad total?
-- No es posible determinar la durabilidad de una lista infinita

-- ¿Qué se obtiene como respuesta cuando se lleva adelante una misión sobre ella?
-- No se puede aplicar una estrategia a una lista infinita ya que algunas dependen de la durabilidad de las naves

