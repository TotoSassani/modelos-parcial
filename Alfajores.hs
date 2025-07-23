{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use notElem" #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Evaluate" #-}
import Data.List (isPrefixOf, isSuffixOf,isInfixOf)

data Alfajor = Alfajor{
    capasRelleno :: [Relleno],
    peso :: Float,
    dulzor :: Float,
    nombre :: String
} deriving (Eq,Show,Ord)

data Relleno = DulceDeLeche | Mousse | Fruta deriving (Eq,Show,Ord)

alfajorJorgito  = Alfajor [DulceDeLeche] 80  8 "Jorgito"
alfajorHavanna = Alfajor [Mousse , Mousse] 60 12 "Havanna"
alfajorCapitanDelEspacio = Alfajor [DulceDeLeche] 40 12 "Capitan del Espacio"

coeficienteDulzor :: Alfajor -> Float
coeficienteDulzor alfajor = dulzor alfajor / peso alfajor

precioAlfajor :: Alfajor -> Float
precioAlfajor alfajor =  peso alfajor * 2 + sum (map preciosDeCapas (capasRelleno alfajor))
--precioAlfajor :: Alfajor -> Float
--precioAlfajor alfajor = peso alfajor * 2 + sum . map preciosDeCapas $ capasRelleno alfajor

preciosDeCapas :: Relleno -> Float
preciosDeCapas DulceDeLeche = 12
preciosDeCapas Mousse = 15
preciosDeCapas Fruta = 10
preciosDeCapas _ = 0

esPotable :: Alfajor -> Relleno -> Bool
esPotable alfajor relleno = all (== relleno ) (capasRelleno alfajor) && coeficienteDulzor alfajor >= 0.1 && capasRelleno alfajor /= []

esPotable' :: Alfajor -> Bool
esPotable' alfajor = all (== head (capasRelleno alfajor) ) $ capasRelleno alfajor
    --  (==carne) $ [carne,papa,huvo] 

abaratarAlfajor :: Alfajor -> Alfajor
abaratarAlfajor alfajor = alfajor {peso = peso alfajor - 10, dulzor = dulzor alfajor - 7}

cambioNombre :: String ->Alfajor -> Alfajor
cambioNombre nuevo alfajor = alfajor {nombre = nuevo}

agregarCapa :: Relleno -> Alfajor -> Alfajor
agregarCapa capaNueva alfajor = alfajor{capasRelleno =  capaNueva : capasRelleno alfajor}

hacerPremium :: Alfajor -> Alfajor
hacerPremium alfajor
    |esPotable alfajor (head (capasRelleno alfajor)) = alfajor{capasRelleno = head (capasRelleno alfajor) : capasRelleno alfajor, nombre = nombre alfajor ++ " premium"}
    |otherwise = alfajor


premiumVariasVeces :: Float -> Alfajor -> Alfajor
premiumVariasVeces 1 alfajor = hacerPremium alfajor
premiumVariasVeces n alfajor = hacerPremium (premiumVariasVeces (n-1) alfajor)

jorgitoAbaratado :: Alfajor
jorgitoAbaratado = cambioNombre "Jorgitito" . abaratarAlfajor $ alfajorJorgito

alfajorJorgelin :: Alfajor
alfajorJorgelin = cambioNombre "Jorgelin" . agregarCapa DulceDeLeche $ alfajorJorgito

capitanDelEspacioCostaACosta :: Alfajor
capitanDelEspacioCostaACosta = cambioNombre "Capitan del Espacio Costa a Costa" . premiumVariasVeces 4 . abaratarAlfajor $ alfajorCapitanDelEspacio

data Clientes = Clientes {
    nombreDelCliente :: String,
    plataParaGastar :: Float,
    alfajoresComprados :: [Alfajor],
    paraGustosLosColores :: Alfajor -> Bool
}

emi = Clientes {
    nombreDelCliente = "Emi",
    plataParaGastar = 120,
    alfajoresComprados = [],
    paraGustosLosColores = isInfixOf "Capitan del Espacio" . nombre
    }

toto = Clientes{
    nombreDelCliente = "Toto",
    plataParaGastar = 100,
    alfajoresComprados = [],
    paraGustosLosColores = \alfajor -> isInfixOf "premium" (nombre alfajor)  && (0.15 <= coeficienteDulzor alfajor)
}

dante = Clientes{
    nombreDelCliente = "Dante",
    plataParaGastar = 200,
    alfajoresComprados = [],
    paraGustosLosColores = \alfajor -> all (/= DulceDeLeche) (capasRelleno alfajor) && not (esPotable' alfajor)
    }

juan = Clientes{
    nombreDelCliente = "Juan",
    plataParaGastar = 500,
    alfajoresComprados = [],
    paraGustosLosColores = \alfajor -> isInfixOf "Jorgito" (nombre alfajor) &&
    (0.15 <= coeficienteDulzor alfajor) &&  isInfixOf "premium" (nombre alfajor) && all (/= Mousse) (capasRelleno alfajor)
}


leGustaACliente :: Clientes -> [Alfajor]-> [Alfajor]
leGustaACliente cliente listaAlfajor = filter (paraGustosLosColores cliente) listaAlfajor

comprarAlfajor :: Clientes -> Alfajor -> Clientes
comprarAlfajor cliente alfajor
    |precioAlfajor alfajor <= plataParaGastar cliente = cliente{plataParaGastar = plataParaGastar cliente - precioAlfajor alfajor,
                                     alfajoresComprados = alfajor : alfajoresComprados cliente}
    |otherwise = cliente

comprarAlfajores :: Clientes->[Alfajor] -> Clientes
comprarAlfajores cliente listaAlfajores = foldl comprarAlfajor cliente . leGustaACliente cliente $ listaAlfajores
