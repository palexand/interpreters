% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[ContT] {Continuation monad transformer}

\begin{comment}
\begin{code}

module ContT where
import Basics
import Initial
import MonadT
import ShowMonad
import ContMonad
import ShowUtils
import StateT

\end{code}
\end{comment}

It changes a monad to use the continuation-passing style. 

This is achieved by turning the monad into a function from a
continuation to a computation over some answer type.  
A continuation is simply a function from the value type to a 
computation over the answer type.

\begin{code}

data ContT ans m a 
    = K ((a -> m ans) -> m ans)

unK (K x) = x

instance Functor m 
    => Functor (ContT ans m) 
  where
    fmap f (K m) = K (\c -> m (c . f))

instance Monad m => Monad (ContT ans m) where
	return  x = K (\c -> c x)
        m >>= f = 
            K (\c -> (unK m) (\a -> unK (f a) c))

instance (Monad m, Monad (ContT ans m))
               => MonadT (ContT ans) m where
   lift m = K (\k -> m >>= k)

\end{code}

|ContT| turns a monad into a |ContMonad|, with the operator |callcc|. 
|callcc f| calls the function |f|, passing it the current continuation
as argument.

\begin{code}

instance (Monad m) => 
            ContMonad (ContT ans m) where
 callcc f
      = K ( \ c -> unK (f (\(K a) -> K (\_ -> a c)
		          )
	               ) c
	  )

\end{code}

