%
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[EnvMonad] {Monad with access to an environment}

\begin{comment}
\begin{code}
module EnvMonad ( EnvMonad(..)
		) where

\end{code}
\end{comment}


\begin{code}

class Monad m 
          => EnvMonad r m 
	      where
    inEnv  :: r -> m a -> m a
    rdEnv  :: m r


\end{code}