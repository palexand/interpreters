An example of a While language. 

This language is very similar to the 
language described in \cite{SlonnegerKurtz95}

\begin{comment}
\begin{code}
module While 
    ( pWhile
    ) where
import WLang
import WParser
import WPretty
import MonadTs hiding (applyOp)
import Runable
import Heap
import Table
import Errors
import IOUtils
import qualified Parsec as P
import Lang
import HasName
\end{code}
\end{comment}

Semantics

\begin{code}

instance Syntax Program where
 exec s = run $ meaning s

instance HasName Program where
 name _ = "While"

instance Initial Program where
 initial = Prg "empty" (Block [] Skip)

pWhile = wParser

type Value = Either Integer 
           ( Either Bool
	   ( Either ()
	            Bottom
	   ))


data EV    = VALUE Value
	   | LOC Loc
	   | PROC Procedure
	   | Unbound

type Procedure  = [Value] -> Computation ()

type Computation = ErrorT
                 ( StateT Store 
                 ( EnvT Env
		 ( DebugT
		        IO
		 )))

meaning :: Program -> Computation ()
perform :: Block -> Computation ()
elaborate :: [Decl] -> Computation Env
elab :: Decl -> Computation Env
execute :: Comm -> Computation ()
eval :: Expr -> Computation Value

allocate :: Computation Loc 

meaning (Prg id block) 
    = perform block

perform (Block decls comm) 
    = do { env' <- elaborate decls
	 ; infoEnv "After elaborating decls" env'
	 ; inEnv env' (execute comm)
	 }

elaborate ds 
    = foldr (\d r -> do { env <- elab d
		        ; inEnv env r
			}) rdEnv ds

elab (DConst id e)
    = do { v <- eval e
	 ; extendEnv id (VALUE v)
	 }

elab (DVar id t)
    = do { loc <- allocate 
	 ; extendEnv id (LOC loc)
	 }

elab (DProc id args block)
    = do { let proc = mkProc id args block
	 ; extendEnv id (PROC proc)
	 }
 where mkProc :: Ident -> [Ident] -> Block -> Procedure
       mkProc id args block = \vs -> 
	  if length vs /= length args
	  then errWrongNumberArgs vs args
	  else 
	    do { env <- extendEnvLs args (map VALUE vs)
	       ; infoEnv ("Before block of " ++ id) env
	       ; inEnv env (perform block)
	       }
 
execute (Seq c1 c2) 
    = do { execute c1
	 ; execute c2
	 }

execute (Assign id e)
    = do { v <- eval e
	 ; ev <- lookupEnv id
	 ; case ev of
	    LOC loc -> updateSto loc v
	    _       -> errCannotAssign id
	 }

execute Skip = return ()

execute (While e c) = loop
 where loop = 
	   do { v <- eval e
	      ; case prj v of
		 Just True -> do { execute c
				 ; loop
				 }
		 Just False -> return ()
		 Nothing    -> errExpectedBool v
              }
    
execute (IF e c1 c2) 
     = do { v <- eval e
	  ; case prj v of
	     Just True  -> execute c1
	     Just False -> execute c2
	     Nothing    -> errExpectedBool v
	  }

execute (Declare block) 
    = perform block

execute (Call id args)
     = do { ev <- lookupEnv id
	  ; vs <- sequence (map eval args)
	  ; env <- rdEnv
	  ; infoEnv ("Calling " ++ id) env
	  ; case ev of
	     PROC p -> 
	       do { p vs
		  ; infoDebug $ "Returning from " ++ id
		  }
             _      -> errProcedureNotFound id
	  }

execute (Read id) 
     = do { ev <- lookupEnv id
	  ; case ev of
	     LOC loc -> 
	       do { mPutStrLn "Input?"
		  ; l <- mGetLine
		  ; case tryRead l of
		     Nothing -> errWrongInput l
		     Just (v::Integer) -> 
		         updateSto loc (inj v) 
		  }
	     _ -> errCannotAssign id
	  }

execute (Write e)
     = do { v <- eval e
	  ; mPutStrLn $ show v
	  }


-- eval 

eval (Con n) = returnInj n

eval (Var v) 
    = do { ev <- lookupEnv v
	 ; case ev of
	     VALUE v -> return v
	     LOC   l -> lookupSto l
	     Unbound -> errUnbound v
	 }


eval ETrue = returnInj True
eval EFalse = returnInj False
eval (BinOp e1 op e2)
    = do { v1 <- eval e1
	 ; v2 <- eval e2
	 ; applyOp op v1 v2
	 }

eval (UnOp op e)
    = do { v <- eval e
	 ; applyUnOp op v
	 }

allocate 
    = do { loc <- allocateSto
	 ; return loc
	 }
    
applyOp :: BOp -> Value -> Value -> Computation Value
applyUnOp :: UOp -> Value -> Computation Value


applyOp Add = applyOpInt (+) 
applyOp Sub = applyOpInt (-)
applyOp Mul = applyOpInt (*)
applyOp Dvd = applyOpInt div
applyOp Or  = applyOpBool (||)
applyOp And = applyOpBool (&&)
applyOp BLT = applyOpIntBool (<)
applyOp BGT = applyOpIntBool (>)
applyOp BLE = applyOpIntBool (<=)
applyOp BGE = applyOpIntBool (>=)
applyOp BEQ = applyOpIntBool (==)
applyOp BNEQ = applyOpIntBool (/=)

applyUnOp = undefined

applyOpInt :: (Integer -> Integer -> Integer) ->
	      Value -> Value -> Computation Value
applyOpInt op v1 v2 
    = case prj v1 of
       Nothing -> errExpectedInt v1
       Just n1 -> case prj v2 of
		   Nothing -> errExpectedInt v2
		   Just n2 -> returnInj $ op n1 n2

applyOpBool :: (Bool -> Bool -> Bool) ->
	      Value -> Value -> Computation Value
applyOpBool op v1 v2 
    = case prj v1 of
       Nothing -> errExpectedBool v1
       Just n1 -> case prj v2 of
		   Nothing -> errExpectedBool v2
		   Just n2 -> returnInj $ op n1 n2

applyOpIntBool :: (Integer -> Integer -> Bool) ->
		  Value -> Value -> Computation Value
applyOpIntBool op v1 v2 
    = case prj v1 of
       Nothing -> errExpectedInt v1
       Just n1 -> case prj v2 of
		   Nothing -> errExpectedInt v2
		   Just n2 -> returnInj $ op n1 n2

-- Environment

type Env   = Table EV

lookupEnv :: Ident -> Computation EV
extendEnv :: Ident -> EV -> Computation Env

lookupEnv id 
    = do { env <- rdEnv
	 ; lookupT id env
	 }

extendEnv id ev 
    = do { env <- rdEnv
	 ; return $ addT id ev env
	 }

extendEnvLs ids evs
    = do { env <- rdEnv
	 ; return $ 
	     foldr 
	       (\(id,ev) r -> addT id ev r) env
                   [(id,ev) | (id,ev) <- zip ids evs]
	 }

-- Store 

type Store = Heap Value

updateSto :: Loc -> Value -> Computation ()
lookupSto :: Loc -> Computation Value
allocateSto :: Computation Loc

updateSto loc val = 
    do { sto <- fetch
       ; sto' <- setHeap sto loc val
       ; set sto'
       ; return ()
       }

lookupSto loc =
    do { sto <- fetch
       ; getHeap sto loc
       }

allocateSto =
    do { (sto::Store) <- fetch
       ; (loc,sto') <- newLocH sto
       ; set sto'
       ; return loc
       }

-- 
r fname = 
    do { xs <- readFile fname 
       ; case P.parse wParser fname xs of
	  Left err -> putStrLn $ show $ err
	  Right p -> 
	           do { print p
		      ; putStrLn "Running ... "
		      ; run $ meaning p
		      }
        }

infoEnv :: String -> Env -> Computation ()
infoEnv msg env = 
   do { infoDebug $ msg ++ "\n" 
	                ++ " with env = " ++ showEnv env
      }

showEnv env =
    concat [id ++ "/" ++ show ev ++ " | "
	   | (id,ev) <- values env
	   ]

test p = run $ meaning p

instance Show EV where
 show (VALUE v) = "val " ++ show v
 show (LOC l)   = "loc " ++ show l
 show (PROC p)  = "proc"
 show (Unbound) = "unbound"

\end{code}