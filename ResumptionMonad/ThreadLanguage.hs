module ThreadLanguage where

type Name = String

		  ----------------------
		  --- Command syntax ---
		  ----------------------

--- Top-level program is a collection of communicating subprocesses.

data Prog    = PL [Comm]

data Exp     = Plus Exp Exp | Var Name | Lit Int | GetPID

data BoolExp = Equal Exp Exp | Leq Exp Exp | TrueExp | FalseExp

data Comm =    Assign Name Exp
	  |    Seq Comm Comm
	  |    If BoolExp Comm Comm
	  |    While BoolExp Comm
	  |    Skip
	  |    Print String Exp
	  |    Psem		   --- Semaphore test ("proberen")   : P mutex
	  |    Vsem		   --- Semaphore release ("verlegen"): V mutex
	  |    Inc Name
	  |    Sleep
	  |    Fork Comm
	  |    Broadcast Name
	  |    Receive Name
	  |    Kill Exp


instance Show Prog where
    show (PL [])	   = ""
    show (PL (p:[]))	   = show p
    show (PL (p:ps))	   = show p ++ "\n||\n " ++ show (PL ps)

instance Show Exp where
    show (Plus e1 e2) = show e1++"+"++show e2
    show (Var n)      = n
    show (Lit i)      = show i
    show GetPID	      = "getpid"

instance Show BoolExp where
    show (Equal e1 e2) = show e1 ++ "==" ++ show e2
    show (Leq e1 e2)   = show e1 ++ "<=" ++ show e2
    show TrueExp       = "true"
    show FalseExp      = "false"

instance Show Comm where
    show (Assign n e)	  = n ++ ":=" ++ show e
    show (Seq c1 c2)	  = " "++show c1 ++ " ;\n " ++ show c2
    show (If b c1 c2)	  = "(if " ++ show b ++
			      " then " ++ show c1 ++
			      " else " ++ show c2 ++")"
    show (While b c)	  = "(while " ++ show b ++
				 " do "	 ++ show c ++ ")"
    show (Print msg e)	  = "print '" ++ msg ++ "' " ++ show e
    show Skip		  = "skip"
    show Psem		  = "P(mutex)" -- should generalize to multiple semaphores
    show Vsem		  = "V(mutex)"
    show (Inc x)	  = x ++ ":=" ++ x ++ "+1"
    show Sleep		  = "sleep"
    show (Fork c)	  = "fork(" ++ show c ++ ")"
    show (Broadcast name) = "broadcast(" ++ name ++ ")"
    show (Receive name)	  = "receive(" ++ name ++ ")"
    show (Kill e)   = "kill(" ++ show e ++ ")"

