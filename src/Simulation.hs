module Simulation 
where
    import Text.Show.Functions 
    import Data.List

    data DataVars = DataVars { metersRequested :: Meter, timeBetweenRequests :: Time }
    data SimulationState = SimulationState { actualTime :: Time, tc :: Time, acum :: Meter, nextRequest :: Time } deriving Eq 
    data SimulationResults = SimulationResults { r1 :: Time } deriving (Show, Eq)
    type Time = Float
    type Meter = Int


-- Getters and setters

    setTC :: Time -> SimulationState -> SimulationState 
    setTC value currentState = currentState { tc = value }

    setAcum :: Meter -> SimulationState -> SimulationState
    setAcum value currentState = currentState { acum = value }

    addTC :: Time -> SimulationState -> SimulationState
    addTC value currentState = setTC (tc currentState + value) currentState

    setActualTime :: Time -> SimulationState -> SimulationState 
    setActualTime value currentState = currentState { actualTime = value }

    addActualTime :: Time -> SimulationState -> SimulationState 
    addActualTime value currentState = setActualTime (actualTime currentState + value) currentState

    setNextRequest :: Time -> SimulationState -> SimulationState
    setNextRequest value currentState = currentState { nextRequest = value }

    addNextRequest :: Time -> SimulationState -> SimulationState
    addNextRequest delta currentState = setNextRequest (nextRequest currentState + delta) currentState


-- Config

    furnanceCapacity :: Meter
    furnanceCapacity = 1600

    timePerCicle :: Time
    timePerCicle = 6

    initialState :: SimulationState
    initialState = SimulationState { tc = 0, actualTime = 0, acum = 0, nextRequest = 0 }

    startEvent :: SimulationState -> SimulationState
    startEvent currentState = setActualTime (nextRequest currentState) currentState


-- Helpers

    ciclesNeeded :: Meter -> Int
    ciclesNeeded = ceiling . (/ fromIntegral furnanceCapacity) . fromIntegral 

    mayWait :: Meter -> SimulationState -> SimulationState
    mayWait amountRequested currentState = conditional . setTC (time + timePerCicle) $ currentState
        where excess = amountRequested - furnanceCapacity
              time = actualTime currentState
              conditional = if excess > 0 then wait excess else dontWait

    dontWait :: SimulationState -> SimulationState
    dontWait = setAcum 0

    wait :: Meter -> SimulationState -> SimulationState
    wait excess = (\s -> addTC (fromIntegral . ciclesNeeded . acum $ s) s) . setAcum excess 

    simulationStep :: DataVars -> SimulationState -> SimulationState
    simulationStep dataVars currentState = run . addNextRequest (timeBetweenRequests dataVars) . startEvent $ currentState
        where run s
                | actualTime s > tc s = mayWait (metersRequested dataVars) s
                | otherwise = wait (acum currentState + metersRequested dataVars) s

    fromFinalState :: SimulationState -> SimulationResults
    fromFinalState finalState = SimulationResults { r1 = actualTime finalState } -- TODO


-- Logging

    fieldsAsList :: SimulationState -> [String]
    fieldsAsList aState = map ($ aState) [show . actualTime, show . tc, show . acum, show . nextRequest]

    toTableRegister :: SimulationState -> String
    toTableRegister = wrap wall . intercalate "\t\t" . padLast "\t   " . fieldsAsList
        where wall = "|| "
              wrap wrapper xs = wrapper ++ xs ++ wrapper
              padFirst padding (s:ss) = (s ++ padding) : ss 
              padLast padding = reverse . padFirst padding . reverse

    logState :: SimulationState -> IO () 
    logState currentState = putStrLn $ toTableRegister currentState

    logTable :: IO ()
    logTable = do
        let separator = replicate 61 '='
        putStrLn separator
        putStrLn "|| TIME\t\tTC\t\tACUM\t\tNEXT EVENT ||"
        putStrLn separator


-- IO

    deltaRequestTime :: IO Time
    deltaRequestTime = return 5

    deltaMetersRequested :: IO Meter
    deltaMetersRequested = return 1000

-- main

    simulation :: Time -> SimulationState -> IO SimulationResults
    simulation duration = (logTable >>) . run
        where run currentState
                | actualTime currentState > duration = return . fromFinalState $ currentState
                | otherwise = do requestInterval <- deltaRequestTime
                                 amountRequested <- deltaMetersRequested
                                 logState currentState
                                 let dataVars = DataVars { metersRequested = amountRequested, timeBetweenRequests = requestInterval }
                                 run . simulationStep dataVars $ currentState
