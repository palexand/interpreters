% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Interp] {Interpreter type class}

\begin{comment}
\begin{code}
module Runable ( Runable(..)
	       , Interaction
	       ) where
import SemMonad
import MonadTs
\end{code}
\end{comment}

A monad is in class |Runable| if it is runable. 
Member function |runM| runs the computations embedded in the monad, 
and returns the result as some IO behavior.
We let |runProg| return |IO()| instead of just |String|
to make it possible to support I/O in the interpreted language.

\begin{code}

type Interaction = IO ()
{-
    String ->    -- input
              ( String  -- rest of input
	      , String  -- output
	      )
-}
class Runable m a where
    run :: m a -> Interaction

instance Show (Error a) 
       => Runable Error a 
  where run e = putStr $ "Error: " ++ show e

instance Runable m (Error a) 
       => Runable (ErrorT m) a 
 where
    run (ET mea) = run mea

instance ( Initial e
	 , Runable m a
	 ) => Runable (EnvT e m) a 
 where
  run (ETM em) = 
      run $ em initial

instance ( Initial s
	 , Runable m (a, s)
	 ) => Runable (StateT s m) a 
 where
  run (STM sm) = 
      run $ sm initial

instance ( Monad m
	 , Runable m a
	 ) => Runable (ContT a m) a 
 where
    run (K k) = 
	run (k return)

{-
instance ( Monad m
	 , Runable m (a,IOChannels)
	 ) => Runable (IOT m) a 
 where
    run (IOT m) = 
	(\s -> run (m (initChannels s)) s)
-}
	     
instance ( Runable m (a, DebugInfo)
	 ) => Runable (DebugT m) a 
 where
    run (DT m) = run m
		    

instance (Show a) => 
    Runable Id a 
 where
    run (Id a) = putStr $ show a

instance (Show a) => 
    Runable IO a 
 where
    run a = 
	do{ v <- a
	  ; putStrLn $ "End with value " ++ show v
	  ; return () 
	  }

\end{code}

