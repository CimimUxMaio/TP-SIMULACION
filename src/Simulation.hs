module Simulation
where
    import System.Random

    data EstadoSimulacion = EstadoSimulacion { dia :: Dias }
    data ResultadoSimulacion = ResultadoSimulacion { clientesArrepentidos :: Porcentaje
                                                   , tiempoOciosoDiassTrabajados :: Porcentaje
                                                   , cantidadDeMetrossPromedio :: Metros 
                                                   } deriving Show
    type Metros = Int
    type Dias = Int
    type Porcentaje = Float
    type Simulacion = Dias -> EstadoSimulacion -> IO ResultadoSimulacion


-- IO

    pedidosGenerados :: IO Int
    pedidosGenerados = randomRIO (3, 10) -- TODO

    cantidadDeZocaloPedida :: IO Metros
    cantidadDeZocaloPedida = randomRIO (500, 20000) -- TODO

    checkPorcentaje :: Int -> IO Bool
    checkPorcentaje porcentaje = do
        numeroRandom <- randomRIO (0, 100)
        return (porcentaje > numeroRandom)

    arrepentimiento :: Dias -> IO Bool
    arrepentimiento diasDeEspera
        | diasDeEspera > 15 = return True
        | diasDeEspera > 10 = checkPorcentaje 60
        | diasDeEspera > 5  = checkPorcentaje 20

    pasoDeLaSimulacion :: Int -> Int -> EstadoSimulacion -> IO EstadoSimulacion 
    pasoDeLaSimulacion = undefined

    simulacion :: Int -> Int -> Simulacion
    simulacion cantidadDeHornos ciclosPorDia duracion = run
        where run estadoActual
                | dia estadoActual > duracion = return (calcularResultados estadoActual)
                | otherwise                   = pasoDeLaSimulacion cantidadDeHornos ciclosPorDia estadoActual >>= run


-- Funciones puras

    calcularResultados :: EstadoSimulacion -> ResultadoSimulacion
    calcularResultados estadoFinal = undefined
