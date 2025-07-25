{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

type Poder =  Nave -> Nave

data Nave = Nave {
    nombre :: String,
    durabilidad :: Float,
    escudo :: Float,
    ataque :: Float,
    poderEspecial :: Poder 
}



naveTIEFighter = Nave "TIE Fighter" 200 100 50 movimientoTurbo

naveXWing = Nave "X Wing" 300 150 100 reparacionEmergencia

naveNaveDeDarthVader = Nave "Nave de Darth Vader" 500 300 200 superTurbo

naveMillenniumFalcon = Nave "Millennium Falcon" 1000 500 50 escudoNave

naveAguanteQuilmes = Nave "Aguante Quilmes" 80 1000 50 habilidadNaveAguanteQuilmes

movimientoTurbo :: Poder
movimientoTurbo nave = nave{ataque = ataque nave + 25}

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

durabilidadFlota :: [Nave] -> Float
durabilidadFlota listaNaves = sum (map durabilidad listaNaves) -- sum . map durabilidad 

--activarPoder :: Poder 
--activarPoder nave = (poderEspecial nave) nave 

fueraDeCombate :: Nave -> Bool
fueraDeCombate nave 
    |durabilidad nave == 0 = True
    |otherwise = False


navesDebiles :: Nave -> Bool
navesDebiles nave = escudo nave <= 200

navesConPeligrosidad :: Float -> Nave -> Bool
navesConPeligrosidad n nave = ataque nave > n 

navesFueraDeCombate :: [Nave] -> [Nave] 
navesFueraDeCombate listaNaves  = filter fueraDeCombate  listaNaves

navesFuertes :: Nave -> Bool 
navesFuertes nave = escudo nave >= 400

--minimizaDurabilidad :: Nave -> [Nave] -> Nave 
--minimizaDurabilidad nave listaDeNave = 








