module Simulation where

import Text.Show.Functions
import Control.Monad.ST.Lazy


data StateVars = StateVars { tc :: Time, acum :: Meter }
type Time = Float
type Meter = Int

furnanceCapacity :: Meter
furnanceCapacity = 1600

timePerCicle :: Time
timePerCicle = 6

ciclesNeeded :: Meter -> Int
ciclesNeeded = ceiling . (/ fromIntegral furnanceCapacity) . fromIntegral 

setTC :: Time -> StateVars -> StateVars 
setTC value stateVars = stateVars { tc = value }

setAcum :: Meter -> StateVars -> StateVars
setAcum value stateVars = stateVars { acum = value }

mayWait :: Meter -> Time -> StateVars -> StateVars
mayWait amountRequested time = conditional . setTC (time + timePerCicle)
    where excess = amountRequested - furnanceCapacity
          conditional = if excess > 0 then wait excess else dontWait

dontWait :: StateVars -> StateVars
dontWait = setAcum 0

wait :: Meter -> StateVars -> StateVars
wait excess = (\s -> addTC (fromIntegral . ciclesNeeded . acum $ s) s) . setAcum excess 

addTC :: Time -> StateVars -> StateVars
addTC value stateVars = setTC (tc stateVars + value) stateVars

simulationStep :: Meter -> Time -> StateVars -> StateVars
simulationStep amountRequested time stateVars
    | time > tc stateVars = mayWait amountRequested time stateVars
    | otherwise = wait (acum stateVars + amountRequested) stateVars

timeBetweenRequests :: Time
timeBetweenRequests = 5

metersRequested :: Meter
metersRequested = 1000

simulation :: Time -> StateVars
simulation tf = runSimulation 0 StateVars { tc = 0, acum = 0 }
    where runSimulation startingTime stateVars
            | startingTime > tf = runSimulation (startingTime + timeBetweenRequests) (simulationStep metersRequested startingTime stateVars)
            | otherwise = stateVars
