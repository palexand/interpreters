% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[IOUtils] {Input Output Monad}

\begin{comment}
\begin{code}
module DebugMonad 
          where
import IOMonad

\end{code}
\end{comment}

\begin{code}

class (IOMonad m) => DebugMonad m where
 infoDebug :: String -> m ()

\end{code}
