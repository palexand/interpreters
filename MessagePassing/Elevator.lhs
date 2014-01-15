\begin{code}
{-# OPTIONS_GHC -fglasgow-exts -fno-monomorphism-restriction #-}

module Elevator where

import MP
import Reactive
import Bus

import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Set as S
import Data.List(sort,partition)
\end{code}


\begin{code}

data AppMessages = Pickup Int -- rider requests pickup at floor
                 | DeliverTo Int -- rider request floor
                 | Arrive Int  -- elevator is at floor
                 | Tick -- clock tick
                   deriving (Show,Eq,Ord)

type Messages = BusMessages AppMessages

isClock Tick = True
isClock _ = False

isPickup (Pickup _) = True
isPickup _ = False

isArrive (Arrive _) = True
isArrive _ = False

isDeliverTo (DeliverTo _) = True
isDeliverTo _ = False


\end{code}

\begin{code}

data El = El { efloor :: Int,
               pickups :: S.Set Int,
               deliveries :: S.Set Int,
               direction :: Dir
             } deriving Show

data Dir = Up | Down | Steady deriving Show

currentFloor = gets efloor
currentPickups = gets pickups
currentDeliveries = gets deliveries
currentDir = gets direction

addPickup f = do s <- get
                 cp <- currentPickups
                 put (s { pickups = S.insert f cp})

removePickup f = do s <- get
                    cp <- currentPickups
                    put (s { pickups = f `S.delete` cp})


addDeliver f = do s <- get
                  cd <- currentDeliveries
                  put (s { deliveries = S.insert f cd})

removeDeliver f = do s <- get
                     cd <- currentDeliveries
                     put (s { deliveries = f `S.delete` cd})

putDir d = do s <- get
              put s { direction = d }

switchDir = do d <- currentDir
               s <- get
               put s { direction = invDir d}
  where invDir Up = Down
        invDir Down = Up
        invDir Steady = Steady


moveUp = do f <- currentFloor
            s <- get
            put s { efloor = (f + 1) }

moveDown = do f <- currentFloor
              s <- get
              put s { efloor = (f - 1) }

\end{code}




\begin{code}
delay name 0 a = return () >> a
delay name n a = do busReady "clock" name 
                    onClock name (delay name (n-1) a)
\end{code}


\begin{code}

elevator name = finit `listeningToAs` ("clock",name)
    where finit =
              do busReady "clock" name
                 f
          f = do req <- reqSet
                 paction name <||> daction name <||> caction name
                 f

paction name = onPickup name $ ph
  where ph floor = do tell $ "pickup on " ++ (show floor) ++ "\n"
                      addPickup floor


daction name = onDeliverTo name $ dh
  where dh floor = do tell $ "deliver to " ++ (show floor) ++ "\n"
                      addDeliver floor


caction name = onClock name $ ch
    where ch = do tell $ "elevator got a clock\n" 
                  move name
                  busReady "clock" name

move name =
    do announceMove name
       calculateMove
       executeMove
                    

calculateMove = do d <- currentDir
                   f <- currentFloor
                   req <- reqSet
                   if (S.null req) 
                      then putDir Steady
                      else case d of
                             Up -> when (f == topFloor) switchDir
                             Down -> when (f == bottomFloor) switchDir
                             Steady -> case partition (<f) (S.toList req) of
                                         ([],(x:xs)) -> putDir Up
                                         (x:xs,[]) -> putDir Down
                                         (d,u) | length d > length u -> putDir Down
                                               | otherwise -> putDir Up

executeMove = do dir <- currentDir
                 tell $ "Moving " ++ (show dir) ++ "\n"
                 case dir of
                   Up -> moveUp
                   Down -> moveDown
                   Steady -> return ()


announceMove name =
    do r <- reqSet
       f <- currentFloor
       if f `S.member` r 
          then do appTell ('!':name) (Arrive f)
                  openDoors
                  return ()
          else return ()


openDoors = do f <- currentFloor
               tell $ "Stopping at " ++ (show f) ++ "\n"
               removeDeliver f
               removePickup f
               
                  
reqSet = liftM2 S.union currentPickups currentDeliveries
reqList = do s <- reqSet
             return $ sort $ S.toList s
                        
topFloor = 10
bottomFloor = 0                                  


  
\end{code}




\begin{code}

clock 0 = return ()
clock n = do appTell "!clock" Tick 
             clock (n-1)

onClock name a = (appFilter name isClock) +--> a
  
\end{code}


\begin{code}

onPickup name a = (appFilter name isPickup) ---> \(Tell _ (AppMsg (Pickup floor))) -> (a floor)
onDeliverTo name a = (appFilter name isDeliverTo) ---> \(Tell _ (AppMsg (DeliverTo floor))) -> (a floor)
onArrive name a = (appFilter name isArrive) ---> \(Tell _ (AppMsg (Arrive floor))) -> (a floor)

passenger name elev start end d = pdelay `listeningToAs` ("clock",cfrom)
    where cfrom = "C:" ++ name
          efrom = "E:" ++ name

          -- delay period before passenger requests pick up
          pdelay = delay cfrom d (pinit `listeningToAs` (elev,efrom))

          -- initial period, passenger sends pick up request and then begins listening for clocks and arrivals 
          pinit = do sendPickupReq
                     busReady "clock" cfrom
                     busReady elev efrom
                     prun 0 waitingForPickup

          -- on clocks, keep count of how many clocks have been seen
          -- on arrivals, either 1) step onto elevator and request delivery or 2) report total waiting time and expire
          prun clks arriveReaction = (onClock cfrom (caction clks arriveReaction)) <||>
                                     (onArrive efrom (arriveReaction clks))

          caction clks arriveReaction =
              do busReady "clock" cfrom
                 prun (clks + 1) arriveReaction

          waitingForPickup clks floor =
              do busReady elev efrom
                 if start == floor
                    then sendDeliverReq >> prun clks waitingForDelivery
                    else prun clks waitingForPickup

          waitingForDelivery clks floor =
              if end == floor
                 then tellLn (name ++ ": delivery in " ++ (show clks) ++ " ticks")
                 else do busReady elev efrom
                         prun clks waitingForDelivery

          sendPickupReq = appTell elev (Pickup start)
          sendDeliverReq = appTell elev (DeliverTo end)
                                
\end{code}


\begin{code}

type ElMonad a = Thread Messages (StateT El (Writer String)) a

\end{code}



\begin{code}

run :: [ElMonad ()] -> (El,String)

run threads = runWriter $ execStateT (runMP' threads) initialState
  where initialState = El 0 S.empty S.empty Steady

ts n = [ ("cbus",bus "clock")
       , ("ebus",bus "elev")
       , ("clock",clock n)
       , ("elev",elevator "elev")
       ]

p1 d = ("p1", passenger "nick" "elev" 1 3 d)
p2 d = ("p2", passenger "ilya" "elev" 4 1 d)

test1 = putStrLn $ snd $ run ( (ts 23) ++ [ p1 4 , p2 0 ] )

\end{code}
