% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[StateT] {State Monad Transformer}

The state monad transformer adds state to a monad: data that can be read
or written arbitrarily.

The state is represented by making the monad a function from the
passed-in state to a computation of a pair of the resulting value and
the resulting state.

\begin{comment}
\begin{code}
module StateT( StateT(..)
	     , unSTM
	     , module StateMonad
	     , module Initial
	     )  where
import MonadT
import StateMonad
import ShowMonad
import Initial
\end{code}
\end{comment}

\begin{code}

newtype StateT s m v 
    = STM (s -> m (v,s))

unSTM (STM m) = m

{-
instance Monad m => 
    Functor (StateT s m) 
 where
  fmap f (STM m) = 
    STM (\s -> do{ (x, s') <- m s
		 ; return (f x, s') 
		 } 
	)
-}

instance Monad m => 
    Monad (StateT s m) 
 where
  return v = STM (\s -> return (v,s))
  m >>= f  = 
      STM (\s -> do { (v,s') <- unSTM m s
		    ;  unSTM (f v) s'
	            })

instance ( Monad m
	 , Monad (StateT s m))
    => MonadT (StateT s) m 
 where
   lift m = STM (\s -> m >>= \x -> return (x,s))  


\end{code}

|StateT| turns a monad into a |StateMonad|, 
with the operator |update|.
|update| takes a function that updates 
the current state.

\begin{code}
instance Monad m => 
    StateMonad s (StateT s m) 
 where 
  update f = STM (\s -> return (s, f s))
\end{code}

If we already have had a |StateMonad| and we apply a monad transformer
to it, we still have a |StateMonad| lifting the |update| operation.

\begin{code}
instance ( StateMonad s m
	 , MonadT t m) => 
    StateMonad s (t m) 
 where
  update = lift . update
\end{code}

