module Simulation 
where

    import Text.Show.Functions

    data DataVars = DataVars { metersRequested :: Meter, timeBetweenRequests :: Time }
    data SimulationState = SimulationState { actualTime :: Time, tc :: Time, acum :: Meter, nextRequest :: Time } deriving (Show, Eq)
    type Time = Float
    type Meter = Int

    furnanceCapacity :: Meter
    furnanceCapacity = 1600

    timePerCicle :: Time
    timePerCicle = 6

    ciclesNeeded :: Meter -> Int
    ciclesNeeded = ceiling . (/ fromIntegral furnanceCapacity) . fromIntegral 

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


    initialState :: SimulationState
    initialState = SimulationState { tc = 0, actualTime = 0, acum = 0, nextRequest = 0 }

    startEvent :: SimulationState -> SimulationState
    startEvent currentState = setActualTime (nextRequest currentState) currentState


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



    deltaRequestTime :: IO Time
    deltaRequestTime = return 5

    deltaMetersRequested :: IO Meter
    deltaMetersRequested = return 1000


    logState :: SimulationState -> IO () 
    logState = print


    simulation :: Time -> SimulationState -> IO SimulationState
    simulation duration = run
        where run currentState
                | actualTime currentState > duration = return currentState
                | otherwise = do requestInterval <- deltaRequestTime
                                 amountRequested <- deltaMetersRequested
                                 logState currentState
                                 let dataVars = DataVars { metersRequested = amountRequested, timeBetweenRequests = requestInterval }
                                 run . simulationStep dataVars $ currentState
