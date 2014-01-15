% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[ContT] {Continuation monad transformer}

\begin{comment}
\begin{code}
module MonadTs ( module MonadT
	       , module ErrorT
               , module EnvT
               , module StateT
               , module ContT
               , module IOT
               , module DebugT
	       , module Id
	       , module SemMonad
               ) where
import MonadT
import ErrorT
import StateT
import EnvT
import IOT
import ContT
import DebugT
import Id
import SemMonad
\end{code}
\end{comment}

\section{Liftings}

\subsection{Liftings to ContMonad}


Lift |ContMonad| through |EnvT|
 
\begin{code}

instance (MonadT (EnvT r) m, ContMonad m) =>  
	    ContMonad (EnvT r m) where
 callcc f = 
     ETM (\r -> 
            callcc (\k -> unETM (f (\(ETM a) -> 
       	                ETM (\r -> k (a r)))) r))
	     

\end{code}

Lift |ContMoand| through |StateT|

\begin{code}

instance ( MonadT (StateT s) m
	 , ContMonad m
	 ) => ContMonad (StateT s m) 
 where
  callcc f = STM (\s -> callcc (\k -> unSTM (f (\(STM a) -> 
		    STM (\s1 -> k (a s1)))) s))

\end{code}

lift |ContMoand| through |ErrorT|

\begin{code}

instance ( MonadT ErrorT m
	 , ContMonad m
	 ) => ContMonad (ErrorT m) 
 where
    callcc f = 
	ET (callcc (\k -> unET (f (\(ET a) -> ET (k a)))))

\end{code}

\subsection{Liftings to EnvMonad}

\begin{code}

instance ( Monad m
	 , StateMonad r m) => EnvMonad r m where
    inEnv r m = update (\_ -> r) >>= \ o ->
		m >>= \val ->
		update (\_ -> o) >>= \_ ->
		return val
    rdEnv = update id

\end{code}

Lift |EnvMonad| through |EnvT|

\begin{code}
{-
instance ( EnvMonad r m
	 , MonadT (EnvT r') m
	 ) => EnvMonad r (EnvT r' m) 
    where
 inEnv r (ETM m) = ETM (\r' -> inEnv r (m r'))
 rdEnv = lift rdEnv
-}
\end{code}

Lift |EnvMonad| through |StateT|

\begin{code}

instance (MonadT (StateT s) m, EnvMonad r m) =>
		EnvMonad r (StateT s m) 
 where
    inEnv r (STM m) = STM (\s -> inEnv r (m s))
    rdEnv = lift rdEnv

\end{code}

Lift |ReaderMonad| through |ErrorT|

\begin{code}

instance (MonadT ErrorT m, EnvMonad r m) =>
		EnvMonad r (ErrorT m) where
    inEnv r (ET m) = ET (inEnv r m)
    rdEnv = lift rdEnv

\end{code}

Lift |EnvMonad| through |WriterT|

\begin{code}

{-
instance (MonadT (WriterT s) m, ReaderMonad r m) =>
		ReaderMonad r (WriterT s m) where
    inEnv r (FC m) = FC (inEnv r m)
    rdEnv = lift rdEnv
-}
\end{code}

Lift |EnvMonad| through |ContT|

\begin{code}

instance (MonadT (ContT ans) m, EnvMonad r m) => 
		EnvMonad r (ContT ans m) where
    inEnv r (K c) = K (\k -> inEnv r (c k))
    rdEnv = lift rdEnv

\end{code}

