% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[MonadT] {Monad Transformer}

\begin{comment}
\begin{code}
module MonadT where
\end{code}
\end{comment}

A monad transformer is a type operation that transforms one monad into
another. 
A monad transformer |t| provides a function that lifts computations in |m|
to computations in |t m|.

\begin{code}
class (Monad m, 
       Monad (t m)) => MonadT t m where
 lift :: m a -> t m a

\end{code}
\medskip


The proof obligations (\cite{LiangHudakJones95}) are:
\begin{itemize}
\item If |m| is a monad, then |t m| is a monad (i.e., satisfies the
monad laws).
\item |liftM . unitM| = |unitM|.
\item |liftM (m `bindM` k)| = |liftM m `bindM` (liftM . k)|.
\end{itemize}

\medskip

Default definition of functor for a monad

\begin{code}
instance Monad m => Functor m where
  fmap f m = do { x <- m 
                ; return (f x) 
                }
\end{code}

