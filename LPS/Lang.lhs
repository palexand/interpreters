% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Lang] {Functional Language}

This file contains the modular monadic semantic
definition of a functional language.


\begin{comment}
\begin{code}
module Lang 
    ( Syntax(..)
    , Lang(..)
    , newSyntax
    ) where
import Parsec(Parser, parse)
import HasName
\end{code}
\end{comment}

\begin{code}

class Syntax s where
  exec   :: s -> IO ()

data Lang = 
    forall s . ( Syntax s
	       , HasName s
	       , Show s
	       ) => MkLang s (Parser s)

instance Syntax Lang where
 exec (MkLang s _) = exec s

instance HasName Lang where
 name (MkLang s _) = name s

instance Show Lang where
 show (MkLang s _) = show s


newSyntax :: Lang -> 
	      String -> 
		  String -> IO (Bool, Lang)
newSyntax l@(MkLang s p) src xs = 
    case parse p src xs of
      Left err -> do { print err
		     ; return (False, l)
		     }
      Right s' -> return $ (True, MkLang s' p)




\end{code}
