%
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[ErrMonad] {Monad with an error operation}

\begin{comment}
\begin{code}
module ErrMonad ( ErrMonad(..)
		) where

\end{code}
\end{comment}


\begin{code}

class ( Monad m
      ) => ErrMonad m 
	where 
	 err :: String -> m a

\end{code}