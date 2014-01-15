% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[ContMonad] {Continuation monad}

\begin{comment}
\begin{code}
module ContMonad where
\end{code}
\end{comment}

\begin{code}

class Monad m => ContMonad m where
   callcc :: ((m a -> m b) -> m a) -> m a

\end{code}

In \cite{Wadler92b, LiangHudakJones95} the type is: 
  |callcc :: ((a -> m b) -> m a) -> m a |
