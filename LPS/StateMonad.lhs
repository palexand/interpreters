%
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[StateMonad] {Monad with state}

\begin{comment}
\begin{code}
module StateMonad ( StateMonad(..)
		  ) where

\end{code}
\end{comment}


\begin{code}

class Monad m => StateMonad s m where
        update :: (s -> s) -> m s
        
        -- default methods
        set :: s -> m s            
        set = update . const 

        fetch :: m s
        fetch = update id

\end{code}