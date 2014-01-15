% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[EnvT]{Environment monad transformer}

This section is based on \cite{LiangHudakJones95}

\begin{comment}
\begin{code}

module EnvT( EnvT(..)
	   , unETM
	   )  where
import MonadT
import ShowMonad
import Initial
import EnvMonad

\end{code}
\end{comment}

The environment monad transformer adds an environment to a monad. 
It consists in data that can be read but not altered, but can be set 
to an arbitrary value over a syntactically-limited scope.

The environment is represented by making the monad a function from the
passed-in environment to the resultant computation.

\begin{code}
newtype EnvT r m v = ETM (r -> m v)

instance Monad m 
         => Functor (EnvT s m) where
  fmap f (ETM m) = 
      ETM (\r -> 
           do { x <- m r
              ; return (f x) 
              } 
	  )

instance Monad m => Monad (EnvT s m) where
  return v = 
      ETM (\r -> return v)

  m >>= f  = 
      ETM (\r -> do { v <- unETM m r
		    ; unETM (f v) r
		    })

unETM::(Monad m)=>EnvT r m a -> (r -> m a)
unETM (ETM m) = m

instance (Monad m, 
	  Monad (EnvT r m))
             => MonadT (EnvT r) m 
 where
   lift m = ETM (\r -> m >>= \x -> return x)

\end{code}


\begin{code}

instance Monad m 
          => EnvMonad r (EnvT r m) 
	      where
 inEnv r m = ETM (\_ -> unETM m r)
 rdEnv     = ETM (\r -> return r)

\end{code}


