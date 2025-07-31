{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
import System.Win32 (xBUTTON1)
{-# HLINT ignore "Eta reduce" #-}
data Chofer = Chofer{
    nombre :: String,
    kilometraje :: Float,
    viajesTomados :: [Viaje],
    condicionesViajes:: CondicionViaje
}

data Viaje = Viaje{
    fecha :: Float,
    clienteTomado :: Cliente,
    costoViaje :: Float
}deriving (Eq,Show)

data Cliente = Cliente{
    nombreCliente :: String,
    dondeVive :: String
}deriving (Eq,Show)

type CondicionViaje = Viaje -> Bool

cualquierViaje :: CondicionViaje
cualquierViaje _ = True

costoViajeMayorA200 :: CondicionViaje
costoViajeMayorA200 viaje = (200 <) $ costoViaje viaje

nombreClienteMayorAN :: Int -> CondicionViaje
nombreClienteMayorAN cantLetras  viaje = (cantLetras < ) . length . nombreCliente  $ clienteTomado viaje

clienteNoVivaEn :: String -> CondicionViaje
clienteNoVivaEn lugar viaje = (/= lugar) . dondeVive $ clienteTomado viaje

lucas = Cliente {dondeVive="Victoria" , nombreCliente="Lucas"}

choferDaniel = Chofer{nombre = "Daniel", kilometraje = 23500, viajesTomados = [viajeLucas], condicionesViajes = clienteNoVivaEn "Olivos"}

viajeLucas = Viaje{fecha= 20042017, clienteTomado = lucas , costoViaje =150}

choferAlejandra = Chofer { nombre = "Alejandra",kilometraje = 180000, viajesTomados = [], condicionesViajes = cualquierViaje}

puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje viaje chofer  = condicionesViajes chofer viaje


liquidacion :: Chofer -> Float
liquidacion chofer = sum. map costoViaje $ viajesTomados chofer

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje choferes =  efectuarElViaje viaje . elQueMenosTiene .filter (puedeTomarViaje viaje) $ choferes

elQueMenosTiene :: [Chofer] -> Chofer
elQueMenosTiene [x] = x
elQueMenosTiene (x:xs:xss) 
    |length (viajesTomados x) >= length (viajesTomados xs) = elQueMenosTiene (x:xss)
    |otherwise = elQueMenosTiene (xs:xss)

efectuarElViaje :: Viaje -> Chofer -> Chofer
efectuarElViaje viaje chofer  = chofer{viajesTomados = [viaje] ++ viajesTomados chofer}

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

viajeLucasde50 = Viaje{fecha= 11032017, clienteTomado = lucas , costoViaje =50}


alInfinito = Chofer {kilometraje = 70000, viajesTomados = repetirViaje viajeLucasde50, condicionesViajes = nombreClienteMayorAN 3}

-- no se puede calcular la liquidacion del viaje de Nito porque es una lista de viajes infinitas
-- Si se puede ya que no nos metemos en la variable viajes infinitos y lo unico que hacemos aca es agregar un viaje lo que hace que no se nos 
--cuelge el programa. 



gongNeng arg1 arg2 arg3 = 
     max arg1 . head . filter arg2 . map arg3


-- arg1= Number => arg3 = Number ... arg2= Number -> Bool 


