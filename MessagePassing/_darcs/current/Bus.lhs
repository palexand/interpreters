\begin{code}
{-# OPTIONS_GHC -fno-monomorphism-restriction #-}

module Bus where

import MP
import Reactive

import Data.Set as S
import GHC.Base ( map )

\end{code}


\begin{code}

data BusMessages msg = Tell String (BusMessages' msg)
                    deriving (Show,Eq)

data BusMessages' msg = AppMsg msg
                   | Subscribe String
                   | Unsubscribe String
                   | Ready String
                     deriving (Show,Eq)

appFilter :: String -> (msg -> Bool) -> BusMessages msg -> Bool
appFilter name p (Tell n' (AppMsg msg)) = (name == n') && (p msg)
appFilter _ _ _ = False

busFilter :: String -> (BusMessages' msg -> Bool) -> BusMessages msg -> Bool
busFilter name p (Tell n' msg) = (name == n') && (p msg)

appTell to m = send (Tell to (AppMsg m))

busSubscribe to is = send (Tell to (Subscribe is))
busUnsubscribe to was = send (Tell to (Unsubscribe was))
busReady to is = send (Tell to (Ready is))

instance Ord msg => Ord (BusMessages msg) where
    compare (Tell _ m1) (Tell _ m2) = compare m1 m2

instance Ord msg => Ord (BusMessages' msg) where
    -- real application messages are most preferred
    compare (AppMsg _) _ = LT
    compare _ (AppMsg _) = GT

    -- subscribes are preferred to unsubscribes
    compare (Subscribe _) _ = LT
    compare _ (Subscribe _) = GT

    compare (Unsubscribe _) _ = LT
    compare _ (Unsubscribe _) = GT

    -- then readys
    compare (Ready _) _ = LT
    compare _ (Ready _) = GT

    compare _ _ = EQ

\end{code}

\begin{code}

bus name = bus' name S.empty S.empty

t `listeningToAs` (bname,lname) =
    do tellLn $ "sending subscription " ++ (show (bname,lname))
       busSubscribe bname lname
       x <- t
       busUnsubscribe bname lname
       return x

-- these bus message predicates are defined so that threads sending
-- (non-subscribe) messages to a buffer to which they are not
-- subscribed will block indefinitely
isSub' subs (Subscribe is) = not (S.member is subs)
isSub' subs _ = False
isUnsub' subs (Unsubscribe was) = S.member was subs
isUnsub' _ _ = False
isRdy' subs (Ready rdy) = S.member rdy subs
isRdy' _ _ = False
isSub name subs = busFilter name (isSub' subs)
isUnsub name subs = busFilter name (isUnsub' subs)
isRdy name subs = busFilter name (isRdy' subs)
isAppMsg (AppMsg _) = True
isAppMsg _ = False

bus' name subs notreadys | S.null subs      =
                             bus_sub name subs notreadys
                             -- bus with no subscribers only accepts subscriptions
bus' name subs notreadys | S.null notreadys = 
                             bus_sub name subs notreadys <||> bus_unsub name subs notreadys <||> bus_relay name subs
                             -- can only relay if all listeners are ready
bus' name subs notreadys | otherwise        =
                             bus_sub name subs notreadys <||> bus_unsub name subs notreadys <||> bus_ready name subs notreadys
                             -- can only accept ready messages if we're waiting on listeners 

bus_relay name subs =
    (busFilter ('!':name) isAppMsg) ---> \(Tell _ (AppMsg m)) -> do bus_send name subs m  -- send a message to each listener
                                                                    bus' name subs subs   -- put all the listeners back into the not ready set

-- FIXME: could the order of the sends matter?
bus_send name subs m = bus_send' name (S.elems subs) m
bus_send' name [] m = return ()
bus_send' name (sub:subs) m =
    do appTell sub m
       bus_send' name subs m

bus_sub name subs notreadys =
    (isSub name subs) ---> \(Tell _ (Subscribe is)) -> do tellLn $ name ++ " added: " ++ is
                                                          bus' name (S.insert is subs) (S.insert is notreadys)

bus_unsub name subs notreadys =
    (isUnsub name subs) ---> \(Tell _ (Unsubscribe was)) -> do tellLn $ name ++ " removed: " ++ was
                                                               bus' name (S.delete was subs) (S.delete was notreadys)

bus_ready name subs notreadys =
    (isRdy name subs) ---> \(Tell _ (Ready rdy)) -> do tellLn $ name ++ " heard ready: " ++ rdy
                                                       bus' name subs (S.delete rdy notreadys)

\end{code}
