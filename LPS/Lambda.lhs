% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Lang] {Functional Language}

This file contains the modular monadic semantic
definition of a functional language.


\begin{comment}
\begin{code}
module Lambda 
    ( LSyntax
    , pLambda
    ) where
import SemMonad
import MonadTs
import Fix
import SFunctor
import Interp
import ShowUtils
import IOUtils
import Heap
import Table
import Errors
import Lang
import LLang
import LPretty
import LParser(pExpr)
import Runable
\end{code}
\end{comment}

\begin{code}

instance Syntax LSyntax where
 exec s   = run $ inter s

instance HasName LSyntax where
 name _ = "Lambda"

pLambda = pExpr

type Result     = Computation Value

type Computation = 
         ErrorT
       ( StateT HeapC 
       ( EnvT TableC 
       ( ContT (Error Value, HeapC)
       ( DebugT
         IO
      ))))

type Value	= Either Function
		( Either Integer 
		( Either Bool 
		( Either () 
		  Bottom 
		)))

-- A test function (only to check if it compiles)

inter :: LSyntax -> Result
inter = interp

runCode :: Result
runCode = do{ mPutStrLn "test"
	    ; returnInj ()
	    }

newtype Function = Fn (Result -> Result)
unFn (Fn x) = x

instance Show Function 	
  where 
     showsPrec p _ = showString "<Function>"

instance HasName Function 
  where name _	= "Function"

instance Initial Function 
  where initial	= Fn id

instance SubType 
          (Result -> Result) Value 
  where
    inj = inj  . Fn
    prj = maybe Nothing 
	        (Just . unFn) . prj

\end{code}

Table of computations

\begin{code}

data TableC 	= TC (Table Result)
unTC  (TC t) = t

instance EnvMonad 
           (Table Result) Computation
 where
  rdEnv = fmap unTC rdEnv
  inEnv = inEnv . TC

instance Initial TableC where 
  initial = 
      TC initial

instance Show TableC 
    where show t = "<<Table>>"

\end{code}

Heap of computations

\begin{code}

data HeapC 	= HC (Heap Result)
unHC  (HC h) = h


instance StateMonad (Heap Result) Computation
 where
  update f = fmap unHC (update (HC . f . unHC))

instance Initial HeapC 
    where initial = HC initial

instance Show HeapC 
    where showsPrec p = 
	      shows . unHC

\end{code}

\subsection{Semantics}

\begin{code}

-- Undef
instance AlgebraC U Result 
 where
  phi Undef = 
      err "Evaluationg undefined functor"

-- Arithmetic
instance AlgebraC N Result 
 where 	
   phi (Num n)       = returnInj n
   phi (mx `Add` my) = evalAndApply add mx my
   phi (mx `Sub` my) = evalAndApply sub mx my
   phi (mx `Mul` my) = evalAndApply mul mx my
   phi (mx `Dvd` my) = 
       do { (x,y)   <- evalOperands mx my
	  ; (px,py) <- prjOperands  x  y
	  ; if (py == 0) 
	    then err (showI px ++ " divided by " ++ 
		          showI 0) 
	    else applyOp dvd px py 
	  } 

type BinOp2 = BinOp Integer Integer

add 	= (+)   :: BinOp2
sub 	= (-)   :: BinOp2
mul 	= (*)   :: BinOp2
dvd 	= div   :: BinOp2
showI   = show  :: Integer -> String


-- Variables
instance AlgebraC V Result
 where 	phi (Var id) = lookupStr id 

instance AlgebraC B Result 
 where
  phi (BCons b)       = returnInj b
  phi (mx `And` my)   = evalAndApply (&&)  mx my
  phi (mx `Or`  my)   = evalAndApply (||)  mx my
  phi (Cond mx my mz) = 
      do { v 	<- mx
	 ; cond <- checkType v
	 ; if cond then my
	   else mz 
	 } 

-- CMP 
instance AlgebraC CMP Result 
 where
  phi (mx `CLT`  my) = evalAndApply (lt)  mx my
  phi (mx `CLE`  my) = evalAndApply (le)  mx my
  phi (mx `CGT`  my) = evalAndApply (gt)   mx my
  phi (mx `CGE`  my) = evalAndApply (ge)  mx my
  phi (mx `CEQ`  my) = evalAndApply (eq)  mx my
  phi (mx `CNEQ` my) = evalAndApply (neq)  mx my

-- avoid unresolved overloading
type BinOpCMP = BinOp Integer Bool

lt  = (<)  :: BinOpCMP
le  = (<=) :: BinOpCMP
gt  = (>)  :: BinOpCMP
ge  = (>=) :: BinOpCMP
eq  = (==) :: BinOpCMP
neq = (>=) :: BinOpCMP

-- Local declarations
instance AlgebraC D Result 
 where	phi = phiD

phiD (LetV x md mb) = 
    do { loc  <- alloc md
       ; tab  <- rdEnv
       ; tab' <- updateTM (x, lookupLoc loc) tab
       ; v    <- inEnv tab' md
       ; updateLoc loc (return v)
       ; v'   <- inEnv tab' mb
       ; return v'	
       }

phiD (LetN x md mb) = 
    do { loc  <- alloc md
       ; tab  <- rdEnv
       ; tab' <- updateTM (x, lookupLoc loc) tab
       ; updateLoc loc (inEnv tab' md)
       ; v'   <- inEnv tab' mb
       ; return v'	
       }

phiD (LetL x md mb) = 
    do { loc  <- alloc md
       ; tab  <- rdEnv
       ; tab' <- updateTM (x, lookupLoc loc) tab
       ; updateLoc loc (createThunk loc (inEnv tab' md))
       ; v'   <- inEnv tab' mb
       ; return v'	
       }

instance AlgebraC F Result
 where 	
   phi (LambdaV x exp) = 
       do { tab <- rdEnv
	  ; returnInj 
	     (\m -> 
	        do { v <- m  
		 -- Evaluates before calling
	           ; tab' <- updateTM (x, return v) tab
		   ; inEnvT tab' exp 
                   })
 	     } 

   phi (LambdaN x exp) = 
	do { tab <- rdEnv
           ; returnInj 
	      (\m -> 
	         do { tab' <- updateTM (x, m) tab
                    ; inEnvT tab' exp 
		    }
	      ) 
           } 

   phi (LambdaL x exp) = 
    do { tab <- rdEnv
       ; returnInjF 
          (\m -> 
   	     do { loc <- alloc m  
  -- inserts m in the heap and inmediately replaces it by 
  -- the thunk.
  -- The problem is that the thunk needs loc and
  -- that alloc needs a value of type (m v) to avoid
  -- unresolved constraints and ambiguous signatures	
                ; updateLoc loc (createThunk loc m)
		; tab' <- updateTM (x, lookupLoc loc) tab
                ; inEnvT tab' exp 
		}) 
	   }
  -- Study Possible space leak 
  -- (when does it remove the created thunk?)

   phi (App mf mx) = 
       do { vf <- mf 
          ; f <- checkType vf
	  ; tab <- rdEnv
	  ; f (inEnvT tab mx) 
	  } 


inEnvT :: Table Result -> Result -> Result	
inEnvT = inEnv

createThunk:: Loc -> Result -> Result
createThunk loc m = do { v <- m
                       ; updateLoc loc (return v)
		       ; return v 
     	     	       }	

instance AlgebraC R Result
 where 
   phi (New mx) = 
    do { val <- mx
       ; loc      <- alloc (return val)
       ; returnInj loc
       } 
	
   phi (Get mx) =
    do { loc    <- mx
       ; chkLoc <- checkType loc
       ; lookupLoc chkLoc
       }
                        
   phi (Assign mx my) =
    do { loc    <- mx
       ; chkLoc <- checkType loc
       ; val    <- my
       ; updateLoc chkLoc (return val)
       ; return val 
       }          

   phi (Seq mx my) =
    do { mx
       ; my 
       }

alloc:: Result -> Computation Loc
alloc mv = 
    do { h  <- fetch
       ; (l,h') <- allocH mv h
       ; set h'
       ; return l
       } 

lookupLoc :: Loc -> Result
lookupLoc loc = 
    do { heap <- fetch
       ; (mv::Result) <- getHeap heap loc
       ; v <- mv
       ; return v
       }

updateLoc:: Loc -> Result -> Computation ()
updateLoc l mv = 
    do{ h <- fetch
      ; h' <- setHeap h l mv
      ; set h'
      ; return ()
      }

instance AlgebraC TIO Result 
 where 
   phi (Read mx) = 
     do { mPutStrLn "Input?"
        ; l <- mGetLine
        ; case tryRead l of
  	   Nothing -> errWrongInput l
	   Just v  -> returnInj (v::Integer)
	}
	
   phi (Write mx) =
     do { v    <- mx
        ; mPutStrLn ("Value: " ++ show v) 
        ; returnInj ()
        }

instance AlgebraC Callcc Result
 where 
   phi Callcc = 
    returnInj  
      (Fn (\m ->
            do { v <- m
	       ; (f'::Function) <- checkType v
	       ; callcc 
	           (\k -> ((unFn f') 
			     (return $ inj (Fn k))))
	       }
	 ))

instance AlgebraC P Result
 where 	phi = phiP

phiP (Print var)  = 
    do { l   <- lookupStr var
       ; v  <-  lookupVal l
       ; mInfo var l v
       ; return v
       } 

phiP (ShowMsg msg) = 
    do { mPutStrLn msg
       ; returnInj () 
       }

lookupStr:: String -> Result
lookupStr name = 
    do { table <- rdEnv
       ; lookupTM name table 
       }

lookupVal :: 
    Value -> Result
lookupVal var = 
 do { loc <- checkType var
    ; val <- lookupLoc loc
    ; return val
    }


\end{code}





