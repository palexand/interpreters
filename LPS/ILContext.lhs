%
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Context] {Context}

\begin{comment}
\begin{code}
module ILContext 
    ( ILMonad
    , ILValue
    , ILCompute
    , ILCont
    , Context(..)
    , initialContext
    , showContext
    ) where
import Stack
import HashTable
import Memory
import Initial
import MonadTs
\end{code}
\end{comment}

\subsection{Intermediate Language Monad}


\begin{code}
type ILMonad = ErrorT 
             ( StateT Context
--	     ( EnvT Context
	     ( ContT (Error ILValue, Context)
	     ( DebugT
	       IO
	     )))--))

type ILValue = Either () Bottom

type ILCompute = ILMonad ILValue

type ILCont = ILCompute -> ILCompute

\end{code}

\subsection{Execution Context}

\begin{code}

data Context = C { stack  :: Stack     Integer     
		 , table  :: HashTable String Integer
		 , memory :: Memory    Integer
		 , memIx  :: Integer
		 , labels :: HashTable String ILCont
		 }

initialContext = C { stack  = emptyStack
		   , table  = emptyTable
		   , memory = initMem (0::Integer)
		   , memIx  = 0
		   }

instance Initial Context 
    where initial = initialContext

instance Show Context where
  show c = showContext c

showContext :: Context -> String
showContext c = 
    "Stack = " ++ show (stack c) ++ "\n" ++
    "Memory = " ++ showStack 
		    (memory c) (memIx c) ++ "\n" 

\end{code}