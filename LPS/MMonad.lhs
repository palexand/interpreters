A monad can be declared as:

\begin{code}
class MMonad m where
   unitM :: a -> m a
   bindM :: m a -> (a -> m b) -> m b
   runM  :: m a -> IO a
\end{code}

|unitM a| places the value |a| into the monad.  |m1 `bindM` \v -> m2|
binds two monads together, passing the result of the first monad to the
second.  |runM m| `runs' a monad, converting it into an I/O monad
returning the same value.

\medskip

A very useful monad operation is |nop|, which returns the empty tuple
|()|.  We define it here, since all our monads should have it.

\begin{code}
nop   :: MMonad m => m ()
nop   =  unitM ()
\end{code}

Note that this is |MMonad| (and |unitM|, |bindM|, |runM| etc.)\ to avoid
conflicts with the |Monad| declaration in the prelude.  We want to
explicitly define our monads in this file.

\medskip

As well as the above types, we have three proof obligations~\cite{Wadler92:Essence}:
\begin{itemize}
\item |(unitM a) `bindM` k = k a|. \hfill(\emph{left unit})
\item |m `bindM` unitM = m|.\hfill(\emph{right unit})
\item |    m `bindM` (\a -> (k a) `bindM` (\b -> h b))|\\
      |=  (m `bindM` (\a -> k a)) `bindM` (\b -> h b)|.
      \hfill(\emph{associative})
\end{itemize}



