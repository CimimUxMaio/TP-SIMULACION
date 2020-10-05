module Simulation
where
    import System.Random

    data EstadoSimulacion = EstadoSimulacion { instante                    :: Horas
                                             , tiempoComprometido          :: Horas 
                                             , proximoPedido               :: Horas
                                             , cantidadPedida              :: Metros
                                             , sumatoriaProduccionEfectiva :: Metros
                                             , sumatoriaProduccionIdeal    :: Metros
                                             , sumatoriaCiclosPorPedido    :: Int
                                             , sumatoriaTiempoOcioso       :: Horas
                                             , pedidosTotales              :: Int
                                             , numeroDeArrepentidos        :: Int
                                             }

    instance Show EstadoSimulacion where
        show estado = "INSTANTE: " ++ (show . instante $ estado) ++ "h, \n"
                   ++ "TIEMPO COMPROMETIDO: " ++ (show . tiempoComprometido $ estado) ++ "h, \n"
                   ++ "PROXIMO PEDIDO: " ++ (show . proximoPedido $ estado) ++ "\n"

        
    data ResultadoSimulacion = ResultadoSimulacion { arrepentidos                   :: Porcentaje
                                                   , tiempoOcioso                   :: Porcentaje
                                                   , produccionSobreProduccionIdeal :: Porcentaje
                                                   , ciclosPorPedido                :: Float
                                                   }

    instance Show ResultadoSimulacion where
        show resultados = "RESULTADOS:\n"
                       ++ "\t- Porcentaje de arrepentidos: " ++ (show . arrepentidos $ resultados) ++ " %\n"
                       ++ "\t- Porcentaje de tiempo ocioso: " ++ (show . tiempoOcioso $ resultados) ++ " %\n"
                       ++ "\t- Porcentaje producido por sobre la produccion ideal: " ++ (show . produccionSobreProduccionIdeal $ resultados) ++ " %\n"
                       ++ "\t- Promedio de ciclos de produccion realizados por pedido: " ++ (show . ciclosPorPedido $ resultados) ++ " ciclos\n"

    type Metros = Float
    type Dias = Int
    type Horas = Float
    type Porcentaje = Float
    type Simulacion = Dias -> EstadoSimulacion -> IO ResultadoSimulacion

-- CONSTANTES
    
    duracionDeUnCiclo :: Horas
    duracionDeUnCiclo = 3

    capacidadDeUnHorno :: Metros
    capacidadDeUnHorno = 1000


-- IO

    intervaloEntrePedidos :: IO Horas
    intervaloEntrePedidos = funcionR <$> randomIO
        where funcionR r = (20.4075 * ((1 / (1 - r)) ** (1 / 5.6603) - 1)) ** (1 / 1.361) 

    cantidadDeZocaloPedida :: IO Metros
    cantidadDeZocaloPedida = funcionR <$> randomIO
        where funcionR r = (736.28 / (((1 - r) / r) ** (1 / 1.2745))) + 173.62 
    

    checkPorcentaje :: Int -> IO Bool
    checkPorcentaje porcentaje = do
        numeroRandom <- randomRIO (0, 100)
        return (porcentaje > numeroRandom)

    arrepentimiento :: Dias -> IO Bool
    arrepentimiento diasDeEspera
        | diasDeEspera > 15 = return True
        | diasDeEspera > 10 = checkPorcentaje 60
        | diasDeEspera > 5  = checkPorcentaje 20
        | otherwise         = return False

    pasoDeLaSimulacion :: Int -> EstadoSimulacion -> IO EstadoSimulacion 
    pasoDeLaSimulacion cantidadDeHornos = (procesarPedido cantidadDeHornos =<<) . (determinarPedido cantidadDeHornos =<<) . determinarProximoEvento . comenzarEvento

    procesarPedido :: Int -> EstadoSimulacion -> IO EstadoSimulacion
    procesarPedido cantidadDeHornos estado
        | tiempoComprometido estado < instante estado = atenderPedido cantidadDeHornos estado
        | otherwise = programarPedido cantidadDeHornos estado

    atenderPedido :: Int -> EstadoSimulacion -> IO EstadoSimulacion
    atenderPedido cantidadDeHornos = return . atencionPedido cantidadDeHornos 
        where atencionPedido cantidadDeHornos estado = hacerEfectivoElPedido . incrementarTiempoOcioso intervaloOcioso . setTiempoComprometido nuevoTiempoComprometido $ estado
                where nuevoTiempoComprometido = instante estado + duracionDeAtencion cantidadDeHornos estado
                      intervaloOcioso = instante estado - tiempoComprometido estado

    programarPedido :: Int -> EstadoSimulacion -> IO EstadoSimulacion
    programarPedido cantidadDeHornos estado = arrepentimiento diasDeEspera >>= (return . programacionPedido)
        where diasDeEspera = horasADias $ tiempoComprometido estado - instante estado
              programacionPedido seArrepiente
                | seArrepiente = incrementarNumeroDeArrepentidos estado
                | otherwise = hacerEfectivoElPedido . setTiempoComprometido nuevoTiempoComprometido $ estado
                where nuevoTiempoComprometido = tiempoComprometido estado + duracionDeAtencion cantidadDeHornos estado
 
    determinarProximoEvento :: EstadoSimulacion -> IO EstadoSimulacion
    determinarProximoEvento estado = intervaloEntrePedidos >>= (return . setProximoEvento estado)

    determinarPedido :: Int -> EstadoSimulacion -> IO EstadoSimulacion
    determinarPedido cantidadDeHornos estado = do 
        cantidad <- cantidadDeZocaloPedida
        let ciclos = ciclosNecesarios cantidadDeHornos cantidad
        print ciclos
        return (setCantidadPedida cantidad . incrementarProduccionIdeal cantidad . incrementarCiclosPorPedido ciclos  $ estado)
        

    simulacion :: Int -> Simulacion
    simulacion cantidadDeHornos duracion = run
        where run estadoActual
                | (horasADias . instante $ estadoActual) > duracion = return (calcularResultados estadoActual)
                | otherwise = pasoDeLaSimulacion cantidadDeHornos estadoActual >>= logEstado >>= run

    logEstado :: EstadoSimulacion -> IO EstadoSimulacion
    logEstado estado = print estado >> return estado


-- Funciones puras

    calcularResultados :: EstadoSimulacion -> ResultadoSimulacion
    calcularResultados estado = ResultadoSimulacion { arrepentidos = porcentajeDeArrepentidos estado 
                                                    , tiempoOcioso = porcentajeDeTiempoOcioso estado 
                                                    , produccionSobreProduccionIdeal = porcentajeProduccionSobreProduccionIdeal estado
                                                    , ciclosPorPedido = promedioDeCiclosPorPedido estado
                                                    }

    horasADias :: Horas -> Dias
    horasADias horas = ceiling (horas / 24)

    diasAHoras :: Dias -> Horas
    diasAHoras dias = fromIntegral (dias * 24) 

    comenzarEvento :: EstadoSimulacion -> EstadoSimulacion
    comenzarEvento estado =
        estado { instante = proximoPedido estado, pedidosTotales = pedidosTotales estado + 1 }

    setProximoEvento :: EstadoSimulacion -> Horas -> EstadoSimulacion
    setProximoEvento estado intervalo = estado { proximoPedido = instante estado + intervalo }

    setCantidadPedida :: Metros -> EstadoSimulacion -> EstadoSimulacion
    setCantidadPedida cantidad estado = estado { cantidadPedida = cantidad }

    incrementarProduccionIdeal :: Metros -> EstadoSimulacion -> EstadoSimulacion
    incrementarProduccionIdeal cantidad estado = estado { sumatoriaProduccionIdeal = sumatoriaProduccionIdeal estado + cantidad } 

    incrementarCiclosPorPedido :: Int -> EstadoSimulacion -> EstadoSimulacion
    incrementarCiclosPorPedido cantidad estado = estado { sumatoriaCiclosPorPedido = sumatoriaCiclosPorPedido estado + cantidad }

    ciclosNecesarios :: Int -> Metros -> Int
    ciclosNecesarios cantidadDeHornos cantidad = ceiling (cantidad / capacidadTotal)
        where capacidadTotal = fromIntegral cantidadDeHornos * capacidadDeUnHorno
              
    porcentajeDeArrepentidos :: EstadoSimulacion -> Porcentaje
    porcentajeDeArrepentidos estado = (numeroDeArrepentidos estado * 100) `division` pedidosTotales estado

    porcentajeDeTiempoOcioso :: EstadoSimulacion -> Porcentaje
    porcentajeDeTiempoOcioso estado = sumatoriaTiempoOcioso estado * 100 / instante estado

    porcentajeProduccionSobreProduccionIdeal :: EstadoSimulacion -> Porcentaje
    porcentajeProduccionSobreProduccionIdeal estado = (sumatoriaProduccionEfectiva estado * 100) / sumatoriaProduccionIdeal estado

    promedioDeCiclosPorPedido :: EstadoSimulacion -> Float
    promedioDeCiclosPorPedido estado = sumatoriaCiclosPorPedido estado `division` pedidosTotales estado

    division :: Int -> Int -> Float
    division a b = fromIntegral a / fromIntegral b

    setTiempoComprometido :: Horas -> EstadoSimulacion -> EstadoSimulacion
    setTiempoComprometido tc estado = estado { tiempoComprometido = tc }

    incrementarTiempoOcioso :: Horas -> EstadoSimulacion -> EstadoSimulacion
    incrementarTiempoOcioso cantidad estado = estado { sumatoriaTiempoOcioso = sumatoriaTiempoOcioso estado + cantidad }

    incrementarProduccionEfectiva :: Metros -> EstadoSimulacion -> EstadoSimulacion
    incrementarProduccionEfectiva cantidad estado = estado { sumatoriaProduccionEfectiva = sumatoriaProduccionEfectiva estado + cantidad }

    hacerEfectivoElPedido :: EstadoSimulacion -> EstadoSimulacion
    hacerEfectivoElPedido estado = incrementarProduccionEfectiva (cantidadPedida estado) estado 

    incrementarNumeroDeArrepentidos :: EstadoSimulacion -> EstadoSimulacion
    incrementarNumeroDeArrepentidos estado = estado { numeroDeArrepentidos = numeroDeArrepentidos estado + 1 }

    duracionDeAtencion :: Int -> EstadoSimulacion -> Horas
    duracionDeAtencion cantidadDeHornos = (duracionDeUnCiclo *) . fromIntegral . ciclosNecesarios cantidadDeHornos . cantidadPedida

    estadoInicial :: EstadoSimulacion
    estadoInicial = EstadoSimulacion { instante = 0
                                     , tiempoComprometido = 0
                                     , proximoPedido = 0
                                     , cantidadPedida = 0
                                     , sumatoriaProduccionEfectiva = 0
                                     , sumatoriaProduccionIdeal = 0
                                     , sumatoriaCiclosPorPedido = 0
                                     , sumatoriaTiempoOcioso = 0
                                     , pedidosTotales = 0
                                     , numeroDeArrepentidos = 0
                                     } 
