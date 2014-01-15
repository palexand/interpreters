%
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[ILInterp] {Interpeter for Intermediate Language}

\begin{comment}
\begin{code}
module ILInterp (
	        ) where
import ILProgram
import ILContext
import IOUtils
import Array
import Parsec
import Interp
import SubType(returnInj)
import IOMonad
import Runable
\end{code}
\end{comment}


\begin{code}

instance Interp ILMonad Program ILValue 
    where interp = execute

test file = 
    do { result <- parseFromFile parseProgram file
       ; case (result) of
           Left err  -> print err
           Right p   -> do { putStrLn ("Program = " ++ 
				       show p)
			   ; run $ execute p
			   }
       }


\end{code}
