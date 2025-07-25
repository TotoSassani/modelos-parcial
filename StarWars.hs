{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

type Poder =  Nave -> Nave

data Nave = Nave {
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poderEspecial :: Poder 
}


naveTIEFighter = Nave "TIE Fighter" 200 100 50 movimientoTurbo

naveXWing = Nave "X Wing" 300 150 100 reparacionEmergencia

naveNaveDeDarthVader = Nave "Nave de Darth Vader" 500 300 200 superTurbo

naveMillenniumFalcon = Nave "Millennium Falcon" 1000 500 50 escudoNave

movimientoTurbo :: Poder
movimientoTurbo nave = nave{ataque = ataque nave + 25}

superTurbo :: Poder 
superTurbo nave = bajarDurabilidad 45 . movimientoTurbo . movimientoTurbo $ nave

bajarDurabilidad :: Int -> Poder
bajarDurabilidad disminucion nave = nave {durabilidad = durabilidad nave - disminucion}

escudoNave :: Poder  
escudoNave nave = incrementarEscudo 100 . reparacionEmergencia $ nave

incrementarEscudo :: Int -> Poder
incrementarEscudo n nave = nave {escudo = escudo nave + n}

reparacionEmergencia :: Poder 
reparacionEmergencia nave = nave{durabilidad = durabilidad nave + 50, ataque = ataque nave - 30}

















