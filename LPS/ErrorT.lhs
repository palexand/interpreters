% 
% (c) Jose E. Labra . University of Oviedo (2000)
%

\section[ErrorT]{Exception handling monad transformer}

\begin{comment}
\begin{code}
module ErrorT ( ErrorT(..)
	      , unET
	      , module Error
	      ) where
import MonadT
import ShowMonad
import Id
import ErrMonad
import Error

\end{code}
\end{comment}

\begin{code}

data ErrorT m a = ET (m (Error a))

unET (ET x) = x

instance ( Monad m ) => Functor (ErrorT m) 
 where	fmap f (ET m) = 
	    ET (m >>= return . 
		ifError raise (ok . f))

instance ( Monad m ) => Monad (ErrorT m) 
 where
  return  = ET . return . ok
  (ET m) >>= k = ET (m >>= ifError (return . raise) 
		                   (unET . k))

instance ( Monad m
	 , Monad (ErrorT m)
	 ) => MonadT ErrorT m 
 where
  lift m = ET (m >>= (return . ok))

instance ( Monad m) => ErrMonad (ErrorT m) 
  where
      err = ET . return . raise

instance ( ErrMonad m
	 , MonadT t m
	 ) => ErrMonad (t m) 
    where
	err = lift . err


\end{code}

\subsection{Checking Rank-2 polymorphism}

It is possible to define monads as first class values using universally
quantified data type fields (also known as rank-2 polymorphism). 

Note that this feature is not supported in Haskell 98 but is implemented 
as a language extension both in Hugs98 and in GHC.

\begin{code}

data MMonad m = M { unit :: forall a . a -> m a
		  , bind :: forall a b . m a -> (a -> m b) -> m b
		  }

\end{code}

We can also define an error monad as:

\begin{code}
data EMonad m = E { eUnit :: forall a . a -> m a
		  , eBind :: forall a b . m a -> (a -> m b) -> m b
		  , eErr   :: forall a . String -> m a
		  }

\end{code}

Finally, we can lift a single monad to an error monad

\begin{code}

m2e :: MMonad m -> EMonad (ErrorT m)
m2e m@(M u b) = E { eUnit = ET . unit m . ok
                  , eBind = \x f -> 
		     ET (bind m (unET x) 
			  (ifError (unit m . raise) (unET . f)))
		  , eErr   = ET . unit m . raise
		  }

\end{code}

Some tests

\begin{code}

type EList = ErrorT [ ] Int

instance ShowMonad [ ] where 
	showMonad xs = showString "EList: " . 
		       showChar '[' . 
                       foldr1 (\x y -> x . showChar ',' . y) xs . 
                       showChar ']'


k1::EList
k1 = return 3

k2::EList
k2 = err "error"

k3::EList
k3 = ET [return 1, return 2, return 3]

cnv::[Int]->EList
cnv = ET . fmap return

k4::EList
k4 = do { x <- k3; if x == 1 then err "Error 1" 
                             else return (x+3)}

\end{code}
