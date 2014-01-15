%
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[ILProgram] {Intermediate Language Program}

\begin{comment}
\begin{code}
module ILProgram ( pIL
		 ) where
import Array
import List
import Parsec
import ILContext
import HashTable
import Stack
import Array
import Memory
import IOUtils
import Parsec
import qualified ParsecToken as P
import ParsecLanguage
import SemMonad
import qualified Lang as L
import Runable
\end{code}
\end{comment}


\begin{code}

instance L.Syntax Program where
 exec s   = run $ execute s

instance HasName Program where
 name _ = "Intermediate Language"

pIL = parseProgram 

instance Initial Program where
 initial = End

data Program = Seq Ins Program
	     | End

mkProgram :: [Ins] -> Program
mkProgram = foldr (\i p -> Seq i p) End

getInstr :: Program -> Integer -> Ins
getInstr p i = error "getInstr"

parseProgram = do { ps <- many1 parseIns
		  ; return $ mkProgram ps
		  }

execute :: Program -> ILMonad ILValue
execute End = returnInj ()
execute (Seq i p) = do { exec i; execute p} 

instance Show Program where
 show End = ""
 show (Seq i p) = show i ++ " | " ++ show p

\end{code}

\subsection{Instructions}

I use an existential type to be able to work with different
types of instructions in an homogeneus way.

\begin{code}

class Instr i where
  exec :: i -> ILMonad ILValue

data Ins = forall i . ( Instr i
		      , Show i
		      ) => Ins i 

instance Instr Ins where 
   exec (Ins i) = exec i 

instance Show Ins where
   show (Ins i) = show i

class HasString i 
    where getString :: i -> String

class HasInt    i 
    where getInt    :: i -> Integer

data PUSHA = PUSHA String deriving Show
data PUSHC = PUSHC Integer deriving Show
data LOAD  = LOAD deriving Show
data STORE = STORE deriving Show
data ADD   = ADD deriving Show
data SUB   = SUB deriving Show
data MUL   = MUL deriving Show
data DIV   = DIV deriving Show
data INT   = INT   String deriving Show
data JMPLZ = JMPLZ Integer deriving Show
data JMPZ  = JMPZ Integer deriving Show
data GOTO  = GOTO  String  deriving Show
data READ  = READ  String deriving Show
data WRITE = WRITE String deriving Show
data LABEL = LABEL String deriving Show

instance HasString PUSHA 
    where  getString (PUSHA v) = v
instance HasString INT   
    where  getString (INT i)   = i
instance HasString READ  
    where  getString (READ r)  = r
instance HasString WRITE 
    where  getString (WRITE w) = w
instance HasInt    JMPLZ 
    where  getInt    (JMPLZ j) = j
instance HasInt    JMPZ  
    where  getInt    (JMPZ j) = j
instance HasString    GOTO  
    where  getString   (GOTO g) = g
instance HasInt    PUSHC 
    where  getInt    (PUSHC c) = c
instance HasString LABEL 
    where  getString (LABEL v) = v

instance Instr PUSHA where
  exec p =  
    do { c <- getContext
       ; maybe (err ("Variable " ++ 
		 getString p ++ 
		 " not found in memory"))
         (\ v -> 
	     setContext c { stack = push v (stack c) }) 
         (lookupKey (getString p) (table c))
       }


instance Instr PUSHC where
  exec p = 
    do { c <- getContext
       ; setContext c { stack = push (getInt p) (stack c)}
       }

instance Instr LOAD where
  exec p = 
    do { c <- getContext
       ; case (pop (stack c)) of
          Nothing    -> err ("LOAD: Empty stack")
          Just (v,s) -> 
	    case (getVal (memory c) v) of
              Nothing  -> err "getVal: cannot obtain value"
              Just val -> 
	        setContext (c { stack = push val s })
       }


instance Instr STORE where
  exec p =
    do { c <- getContext
       ; case pop2 (stack c) of
          Nothing        -> err ("STORE: empty stack")
          Just (v1,v2,s) -> 
	    maybe (err "setVal: cannot assign")
                  (\mem -> 
                        setContext $ c { stack = s
                                       , memory = mem 
                                       })
                  (setVal (memory c) v2 v1)
          }

execOp op str =
    do{ c <- getContext
      ; case pop2 (stack c) of
          Nothing        -> 
	     err (str ++ ": empty stack")
          
          Just (v1,v2,s) -> 
	      setContext $ c { stack = push (op v1 v2) s 
                             }
      }
 

instance Instr ADD where
  exec p = execOp (+) "ADD"
   
instance Instr SUB where
  exec p = execOp (-) "SUB" 

instance Instr MUL where
  exec p = execOp (*) "MUL"

instance Instr DIV where
  exec p = execOp div "DIV"

instance Instr INT where
  exec i = 
      do { c <- getContext
         ; if memIx c == memSize then err "Out of Memory"
           else setContext $ 
	         c { table = addKey (getString i) 
		                    (memIx c) 
		                    (table c)
                   , memIx = memIx c + 1
		   }
	 }

instance Instr GOTO where
  exec i = do { c <- getContext
	      ; getCont c (getString i) 
	      }


instance Instr JMPLZ where
  exec j = 
      do { c <- getContext
	 ; case pop (stack c) of
	     Nothing -> err "JMPLZ: Empty stack"
	     Just (v,s) -> 
	       if v < 0 
		 then setContext $ 
	                c { stack  = s
			  }
		 else setContext $ 
	                c { stack = s
			  }
	  }

instance Instr JMPZ where
  exec j = 
      do { c <- getContext
	 ; case pop (stack c) of
	     Nothing -> err "JMPZ: Empty stack"
	     Just (v,s) -> 
	       if v == 0 
		 then setContext $ 
	                 c { stack  = s
--			   , iCount = getInt j
			   }
		 else setContext $ c { stack = s
				     }
	 }

instance Instr READ where 
  exec r = 
   do { c <- getContext
      ; mPutStrLn ("Value of " ++ (getString r) ++ " ?")
      ; str <- mGetLine
      ; case tryRead str of
      Nothing -> 
         err "Read: Cannot parse value"
      Just v  -> 
        case lookupKey 
                  (getString r) 
                     (table c) of
	  Nothing   -> 
            err ("READ: Cannot find value of " ++ 
		  getString r ++ 
		  " in table")
	  Just i -> 
             maybe (err "read: cannot assign value in memory")
	           (\mem -> setContext $ 
			      c { memory = mem 
			        })
		   (setVal (memory c) i v)
      }


instance Instr WRITE where 
  exec w = 
       do { c <- getContext
	  ; case lookupKey (getString w) (table c) of
       	       Nothing   -> 
	         err ("WRITE: Cannot find value of " ++ 
                       getString w ++ 
                       " in table")
               Just i -> 
	          maybe 
	            (err 
		      "write: cannot get value in memory")
	            (\val -> 
		        do { mPutStrLn $ 
			     "Value of " ++ 
                             getString w ++ 
                             " = " ++ 
                             show val
			   ; returnInj ()
       			   }) 
			 (getVal (memory c) i)
	   }

instance Instr LABEL where 
  exec w = do{ c <- getContext
	     ; callcc 
	         (\k -> do { setContext $
		               addLabel c (getString w) k
		           })
	     }

addLabel :: Context -> 
	      String -> 
		(ILMonad ILValue -> ILMonad ILValue) ->
		    Context
addLabel c l k = c { labels = addKey l k (labels c) }


getCont :: Context -> String -> ILMonad ILValue
getCont c l = 
    case lookupKey l (labels c) of
	  Nothing -> err "Label not found"
	  Just f  -> err "getCont not implemented..."




getContext :: ILMonad Context
getContext = fetch

setContext :: Context -> ILMonad ILValue
setContext c = do { set c; returnInj () }


-- Parser 
parseIns :: Parser Ins
parseIns = pPUSHA 
       <|> pPUSHC 
       <|> pLOAD 
       <|> pSTORE 
       <|> pADD 
       <|> pSUB 
       <|> pMUL
       <|> pDIV
       <|> pINT   
       <|> pJMPLZ
       <|> pJMPZ 
       <|> pGOTO 
       <|> pREAD  
       <|> pWRITE
       <|> pLABEL

pArgStr res con = 
    do { reserved res
       ; arg <- identifier
       ; return $ Ins $ con arg 
       }

pArgInt res con = 
    do { reserved res
       ; arg <- integer
       ; return $ Ins $ con arg 
       }

pNoArg res con = 
    do { reserved res
       ; return $ Ins $ con
       }


pPUSHA = pArgStr "PUSHA" PUSHA
pLOAD = pNoArg "LOAD" LOAD
pPUSHC = pArgInt "PUSHC" PUSHC
pSTORE = pNoArg "STORE" STORE
pADD = pNoArg "ADD" ADD
pSUB = pNoArg "SUB" SUB
pMUL = pNoArg "MUL" MUL
pDIV = pNoArg "DIV" DIV
pINT = pArgStr "INT" INT
pJMPLZ = pArgInt "JMPLZ" JMPLZ
pJMPZ = pArgInt "JMPZ" JMPZ
pGOTO = pArgStr "GOTO" GOTO
pREAD = pArgStr "READ" READ
pWRITE = pArgStr "WRITE" WRITE
pLABEL = pArgStr "LABEL" LABEL

reserved        = P.reserved lang    
identifier      = P.identifier lang    
integer         = P.integer lang

lang    = P.makeTokenParser 
            (haskellStyle{ 
	       reservedNames = 
	         [ "PUSHA", "PUSHC", "LOAD", "STORE"
		 , "ADD", "SUB", "MUL", "DIV"
		 , "INT", "JMPLZ", "JMPZ", "GOTO"
		 , "READ", "WRITE"
		 ]
	     })


\end{code}