\begin{code}
{-# OPTIONS_GHC -fno-monomorphism-restriction -fglasgow-exts #-}
module MP where

import Data.List
import Control.Monad.Writer
import Test.QuickCheck

import Reactive


data Req msg = Send msg
             | Recv (msg -> Bool)
             | Cont

data Rsp msg = Msg msg
             | Ack

instance Signal (Req msg) where
  cont = Cont
\end{code}

\begin{code}

type Thread msg m a = (String,ReactT (Req msg) (Rsp msg) m a)

-- runMP :: Monad m => [Thread msg m a] -> m ()
runMP' ts = runMP ts
  where runMP ts =
          case getNextEvent (threadSort ts) of
            (SendEvent msg sender@(sname,_) receiver@(rname,_) rest) ->
              do tellLn $ "----- handling sendevent for " ++ (show (sname,rname))
                 sender' <- (reaction sender) Ack
                 receiver' <- (reaction receiver) (Msg msg)
                 runMP (rest ++ [ (sname,sender') , (rname,receiver') ])
            (ContEvent m@(mname,_) ms) ->
                do tellLn $ "----- handling contevent for " ++ mname
                   m' <- (reaction m) Ack
                   runMP (ms ++ [(mname,m')])
            HaltEvent -> do tellLn $ "----- handling halt"
                            return ()

threadSort = sortBy (\x y -> compare (request x)  (request y)) .
             (filter (not . isDone . computation))

instance Eq msg => Eq (Req msg) where
 (Send x) == (Send y) = x == y
 Cont == Cont = True
 _ == _ = False

instance (Eq msg, Ord msg) => Ord (Req msg) where
    compare (Send x) (Send y) = compare x y
    compare Cont _ = LT
    compare _ Cont = GT
    compare (Recv _) _ = LT
    compare _ (Recv _) = GT
--    compare Cont _ = EQ
--    compare _ Cont = EQ
--    compare (Recv _) _ = EQ
--    compare _ (Recv _) = EQ
\end{code}

\begin{code}
data Event msg m a =
    SendEvent msg (Thread msg m a) (Thread msg m a) [Thread msg m a]
  | ContEvent (Thread msg m a) [Thread msg m a]
  | HaltEvent

isSend (SendEvent _ _ _ _) = True
isSend _ = False
isCont (ContEvent _ _) = True
isCont _ = False
isHalt HaltEvent = True
isHalt _ = False

getNextEvent :: [Thread msg m a] -> Event msg m a
getNextEvent [] = HaltEvent
getNextEvent ts =
  case findRest isReady [p | p <- ts, not (isDone (computation p))] of
    Left _ -> -- halted system
      HaltEvent
    Right (t,rst) ->
      case request t of
        (Send msg) -> -- singlecast
          case findRest (canReceive msg) rst of
            Left _ -> getNextEvent rst `requeue` t
            Right (recv,rst') -> SendEvent msg t recv rst'
        (Cont) -> -- step
          ContEvent t rst

findRest p ts = find' p ts [] 
  where find' p [] acc = Left (reverse acc)
        find' p (x:xs) acc | p x = Right (x,(reverse acc)++xs)
                           | otherwise = find' p xs (x:acc) 


-- insure that findRest preserves list lengths. We count as trivial those tests where
-- findRest "fails" by not finding the selected integer in the list. Note that this property
-- test is universal over all element types, due to parametricity

prop_findRestSize :: Int -> [Int] -> Property
prop_findRestSize x lst = (not (x `elem` lst))
                            `trivial` 
                          case findRest ((==)x) lst of
                            Left rst -> (length lst) == (length rst)
                            Right (p,rst) -> (length lst) == ((length rst) + 1)


requeue :: Event msg m a -> Thread msg m a -> Event msg m a
(SendEvent msg s r rest) `requeue` t = SendEvent msg s r (t : rest)
(ContEvent c rest) `requeue` t = ContEvent c (t : rest)
(HaltEvent) `requeue` t = HaltEvent
\end{code}

\begin{code}
reaction :: Thread msg m a -> ( Rsp msg -> m (ReactT (Req msg) (Rsp msg) m a ))
reaction (_,P _ f) = f
reaction (_,D _) = error "reaction requested for completed computation"

request :: Thread msg m a -> (Req msg)
request (_,P req _) = req
request (_,D _) = error "request requested for completed computation"

name :: Thread msg m a -> String
name = fst

computation :: Thread msg m a -> ReactT (Req msg) (Rsp msg) m a
computation = snd

canReceive :: msg -> Thread msg m a -> Bool
canReceive msg t =
    case request t of
      Recv pred -> pred msg
      _ -> False

isReady :: Thread msg m a -> Bool
isReady t =
    case request t of
      Recv _ -> False
      _ -> True
\end{code}

\begin{code}

send s = signal (Send s) >> return ()
hear p = do (Msg rsp) <- signal (Recv p)
            return rsp
\end{code}


These two combinators are an ``event ---> action'' function, and a
"choice'' function over event ---> actions.
\begin{code}

p ---> a = do (Msg m) <- signal (Recv p)
              a m

p +--> a = p ---> (const a)

(P (Recv p1) f1) <||> (P (Recv p2) f2) = P (Recv p') f'
  where p' x = p1 x || p2 x
        f' (Msg msg) = if p1 msg 
                         then  f1 (Msg msg)
                         else f2 (Msg msg)

\end{code}


\begin{code}

loop m = m >> return () >> (loop m)

loopArg m arg = do arg' <- m arg
                   return ()
                   loopArg m arg'
\end{code}



\begin{code}

data AppMsg = Say String
            | Hear 

isSay (Say _) = True
isSay _ = False
unSay (Say x) = x

t1 :: MT
t1 = do send (Say "hello")
        tell $ "t1 done talking" ++ "\n"
        x <- hear isSay
        tell $ "t1 heard a " ++ (unSay x) ++ "\n"
        return ()

t2 :: MT
t2 = do x <- hear isSay
        tell $ "t2 heard a " ++ (unSay x) ++ "\n"


type MT = ReactT (Req AppMsg) (Rsp AppMsg) (Writer String) ()
type MTS = [MT]

tellLn s = tell $ s ++ "\n"  
\end{code}
