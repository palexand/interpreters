module ThreadSemantics where

import Prelude hiding (init)
import MonadicConstructions hiding (u,g)
import ThreadLanguage

                  -------------------------
                  ---  Monad Hierarchy  ---
                  -------------------------
-- the store
type Sto = Name -> Int

type Message  = Int

--- Requests & Responses
data Req = Cont | SleepReq | ForkReq Comm | BcastReq Message | RecvReq | TestReq
         | ReleaseSemReq | GetSemReq  | PrintReq String | GetPIDReq | ExitReq
         | KillReq PID
                                                           deriving Show
data Rsp = Ack | ForkRsp | RecvRsp Message | GetPIDRsp PID deriving Show

--- for instrumentation purposes, we start with the 
--- IO monad as base instead of Id.
type St = StateT Sto Id
type R  = ResT St 
type Re = ReactT Req Rsp St 


etaM = Id
bindM x f = x >>= f

etaSt :: a -> St a
etaSt = return

(*=) :: St a -> (a -> St b) -> St b
(*=) = (>>=)

(!!=) :: R a -> (a -> R b) -> R b
(!!=) = (>>=)

etaR :: a -> R a
etaR = Done

(||=) :: Re a -> (a -> Re b) -> Re b
(||=) = (>>=)

etaRe :: a -> Re a
etaRe = D


                  ------------------
                  ---  Liftings  ---
                  ------------------
rstep :: St a -> R a
rstep x = Pause $ x >>= (return . Done)

step :: St a -> Re a
step x = P (Cont, \ _ -> x >>= (return . D))

                  ----------------------------------
                  ---  Extra Structure within St ---
                  ----------------------------------

{-g :: Monad m => StateT sto m sto-}
g :: St Sto
g = ST (\ u1 -> return (u1,u1))

{-u :: Monad m => (sto -> sto) -> StateT sto m a-}
u :: (Sto -> Sto) -> St a
u delta = ST (\u1 -> return (nil,delta u1))

nil :: a
nil = undefined

write msg = ST (\ s -> {-print msg >> -} return ((),s))

                  -----------------------------
                  ---  Signals and Handler  ---
                  -----------------------------

signal :: Req -> Re Rsp
signal q = P (q, etaSt . etaRe)

signalI :: Req -> Re a
signalI q = P (q, \ _ -> etaSt $ etaRe nil)  


mexp  :: Exp -> Re Int
mexp (Plus e1 e2) = do v1 <- mexp e1
                       v2 <- mexp e2
                       return (v1 + v2) 
mexp (Var n)      = step $ getloc n
    where getloc :: Name -> St Int
          getloc loc = g >>= \ mem -> return (mem loc)
mexp (Lit i)      = return i
mexp GetPID       = signal GetPIDReq ||= \ (GetPIDRsp pid) ->  (return pid)


mbexp :: BoolExp -> Re Bool
mbexp (Equal e1 e2) = do v1 <- mexp e1
                         v2 <- mexp e2
                         return $ v1 == v2                         
mbexp (Leq e1 e2)   = do v1 <- mexp e1
                         v2 <- mexp e2
                         return $ v1 <= v2 
mbexp TrueExp       = return True
mbexp FalseExp      = return False

tweek i v sigma = \ n -> if i==n then v else sigma n
store loc v = (step . u) (tweek loc v)



cmd  :: Comm -> Re a
cmd (Assign id e)  = (mexp e) ||= store id 
cmd (Seq c1 c2)    = cmd c1 >> cmd c2
cmd (If b e1 e2)   = mbexp b ||= \ v -> 
                        if v then (cmd e1)
                             else (cmd e2)
cmd (While b c)    = mwhile (mbexp b) (cmd c)
     where mwhile b phi = b ||= \ v ->
                            if v then phi >> (mwhile b phi) 
                                 else return nil 
cmd Sleep          = signalI SleepReq
cmd (Fork c)       = signalI (ForkReq c)
cmd (Print m e)    = (mexp e) ||= \ v -> signalI (PrintReq (output m v))
       where output m v = m ++ " " ++ show v ++ " "
cmd (Broadcast x)  = (mexp $ Var x) ||= (signalI . BcastReq)
cmd (Receive x)    = signal RecvReq ||= \ (RecvRsp m) -> (store x m)
cmd Psem           = signalI GetSemReq
cmd Vsem           = signalI ReleaseSemReq
cmd (Inc x)        = mexp (Var x) ||= ((store x) . (\ n -> n+1))
cmd Skip           = return nil  
cmd (Kill e)       = (mexp e) ||= \ pid -> signalI (KillReq pid)

prog (PL cs) = map cmd cs

                  ----------------------
                  ---  Schedulers  ---
                  ----------------------

cont :: Monad m => (Rsp -> m (ReactT Req Rsp m a)) -> Rsp -> ReactT Req Rsp m a
cont u rsp = P (Cont, \ Ack -> u rsp)


type Semaphore = Int
type PID = Int
type Thread a = (PID,Re a)

type System a = ([Thread a],[Message],Semaphore,String,PID)

handler :: System a -> Thread a -> R a
handler (ts,mQ,mutex,out,pgen) (pid,D v)              = rrobin (ts,mQ,mutex,out,pgen)
handler (ts,mQ,mutex,out,pgen) (pid,P(Cont,r)) 
                   = Pause $ (r Ack) *= \ k -> etaSt $ next (pid,k) 
          where next t = rrobin (ts++[t],mQ,mutex,out,pgen)
                   
handler (ts,mQ,mutex,out,pgen) (pid,P(SleepReq,r))   = Pause $ etaSt next
     where next = rrobin (ts++[(pid,P(Cont,r))],mQ,mutex,out,pgen)

handler (ts,mQ,mutex,out,pgen) (pid,P(ForkReq c,r))    =
        Pause $ (etaSt next)
     where parent = (pid,cont r ForkRsp)
           child  = (pgen,cmd c)
           next   = rrobin (ts++[parent, child],mQ,mutex,out,pgen+1)

handler (ts,mQ,mutex,out,pgen) (pid,P(BcastReq m,r)) = Pause $ etaSt next
     where mQ' = mQ ++ [m]
           next = rrobin (ts++[(pid,cont r Ack)],mQ',mutex,out,pgen)
     
handler (ts,[],mutex,out,pgen) (pid,P(RecvReq, r))      = Pause $ etaSt next
     where next = rrobin (ts++[(pid,P(RecvReq, r))],[],mutex,out,pgen)
handler (ts,(m:ms),mutex,out,pgen) (pid,P(RecvReq, r))  = Pause $ etaSt next
     where next = rrobin (ts++[(pid,cont r (RecvRsp m))],ms,mutex,out,pgen)

handler (ts,mQ,mutex,out,pgen) (pid,P(PrintReq msg, r)) =
        Pause $ write ({- out ++ ", " ++ -} msg) *= \ _ ->  etaSt next
           where next = rrobin (ts++[(pid,P(Cont,r))],mQ,mutex,out++msg,pgen)

handler (ts,mQ,mutex,out,pgen) (pid,P(GetSemReq, r)) 
        = Pause (etaSt next)
            where next = if mutex>0 then goahead else tryagain
                  goahead  = rrobin (ts++[(pid,P(Cont,r))],mQ,mutex-1,out,pgen)
                  tryagain = rrobin (ts++[(pid,P(GetSemReq,r))],mQ,mutex,out,pgen)

handler (ts,mQ,mutex,out,pgen) (pid,P(ReleaseSemReq, r)) = Pause $  etaSt next
     where next = rrobin (ts++[(pid,cont r Ack)],mQ,mutex+1,out,pgen)

handler (ts,ms,mutex,out,pgen) (pid,P(GetPIDReq, r))  = Pause $ etaSt next
     where next = rrobin (ts++[(pid,cont r (GetPIDRsp pid))],ms,mutex,out,pgen)

handler (ts,ms,mutex,out,pgen) (pid,P(KillReq i, r))  = Pause $ etaSt next
     where next = rrobin (wl',ms,mutex,out,pgen)
           wl'  = filter (exit i) (ts++[(pid,cont r Ack)])
           exit i = \ (pid,t) -> i/=pid
       
     
rrobin :: System a ->  R a
rrobin ([],_,_,_,_)               = Done nil
rrobin ((t:ts),mQ,mutex,out,pgen) = handler (ts,mQ,mutex,out,pgen) t 

live [] = ""
live ((pid,t):ts) = show pid ++";"++ live ts

                  ----------------------------
                  ---  Running the System  ---
                  ----------------------------
                  
runprog :: Prog -> R ()
runprog (PL cs) = rrobin (zip ids (map cmd cs),[],1,"",lcs+1)
    where lcs = length cs
          ids = [1..lcs]

init h = u (\ _ -> h)

run :: R a -> St a
run (Done v)    = etaSt v
run (Pause phi) = phi *= run

takeK :: Int -> R a -> R a
takeK 0 x           = Done (error "uggh")
takeK _ (Done v)    = Done v
takeK n (Pause phi) = Pause (phi *= (etaSt . (takeK (n-1))))

                  ----------------------------
                  ---  Testing the System  ---
                  ----------------------------


go p = (deST (run $ takeK 500 (runprog p))) initSto
     where initSto = \ n -> 0
          
sleep 1 = Sleep
sleep n = Seq Sleep $
          Seq (Print "...sleeping" (Lit 99)) (sleep $ n-1)

brc = While TrueExp (Seq (Inc "x") (Seq (Print "broadcasting " (Var "x")) (Broadcast "x")))

slowbrc = While TrueExp (Seq (Inc "x") 
                        (Seq (Print "broadcasting " (Var "x")) 
                        (Seq (Broadcast "x") (sleep 5))))
         
rcv = While TrueExp 
               (Seq (Receive "y")
                    (Print "  receiving " (Var "y")))

lf = Seq (Fork (While TrueExp (Seq (Inc "x") (Seq (Inc "x") (Print "child is" GetPID)))))
         (While TrueExp (Seq (Inc "x") (Seq (Inc "x") (Print "parent is " GetPID))))
            

lf1 = Seq (Fork (Print "child is" GetPID))
          (Print "parent is" GetPID)
           

acqrel  = Seq Psem $
          Seq (Print "Acquired!\n" (Lit 0)) $
          Seq (sleep 5) $
          Seq Vsem $ sleep 10

acqnorel = Seq Psem $
           Seq (Print "Acquired!\n" (Lit 0)) $
           Seq (sleep 5) $
               sleep 10
                    
tryacq  = Seq (sleep 2) $
          Seq Psem      $
              Print "Got it finally!\n" (Lit 99)
                    
p1 = PL [brc, rcv]
p2 = PL [Print "this is y" (Var "y")]
p3 = PL [Sleep]           
p4 = PL [Seq (Assign "x" (Lit 100)) (Assign "x" (Lit 100)),Sleep]
p5 = PL [cx,cy]
    where cy = While TrueExp (Print "\t this is y" (Var "y"))
          cx = While TrueExp (Seq (Inc "x") (Print "this is x" (Var "x")))
p6 = PL [slowbrc, rcv]
p7 = PL [acqrel,tryacq]
p8 = PL [acqnorel,tryacq]
p9 = PL [forker]
       where forker = While TrueExp lf
p10 = PL [forker]
       where forker = While TrueExp lf1
p11 = PL [Seq lf1 lf1]
p12 = PL [Seq lf1 (Fork Skip)]
p13 = PL [Seq Skip lf1]
