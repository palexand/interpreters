% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[DebugT]{Debugging Monad Transformer}

\begin{comment}
\begin{code}
module DebugT where
import Initial
import ShowMonad
import Basics
import ShowUtils
import StateT
import MonadT
import IOMonad
import DebugMonad
\end{code}
\end{comment}

\begin{code}
newtype DebugT m v = DT (StateT DebugInfo m v)
unDT (DT m) = m

data DebugInfo = 
    DI { count :: Int
       , debug :: Int
       }

updateDbg :: DebugInfo -> DebugInfo
updateDbg d = d { count = count d + 1 } 

instance Show DebugInfo 
 where
   show d = "Debug: Count = " ++ show (count d) 
	    ++ " debugLevel = " ++ show (debug d)


instance Initial DebugInfo 
  where 
    initial = 
	DI { count = 0
	   , debug = 1 -- debug level
	   }

instance (IOMonad m) => 
    Monad (DebugT m) 
 where
  return       = DT . return
  (DT m) >>= k = DT (do { info <- fetch
			; set (updateDbg info)
		        ; v <- m 
			; unDT (k v) 
			} 
		    )

instance (IOMonad m) 
    => StateMonad DebugInfo (DebugT m) 
 where
  update = DT . update


instance ( IOMonad m
         , StateMonad DebugInfo (DebugT m)
	 , IOMonad (DebugT m)
	 ) => DebugMonad (DebugT m) 
 where 
  infoDebug msg = 
      do { d <- fetch
	 ; if debug d > 0 
	   then
	    do { mPutStrLn msg
	       ; opt <- askDebug
	       ; case opt of
		  Step -> return ()
		  NonStop -> 
	            do { set d { debug = 0 } 
		       ; return ()
		       }
	          _    -> return ()
	       }
	    else return ()
	 }

          
instance ( Monad m 
	 , StateMonad DebugInfo (DebugT m)
	 ) => MonadT DebugT m
 where
  lift = DT . lift

instance ( DebugMonad m
	 , IOMonad (t m)
	 , MonadT t m
	 ) => DebugMonad (t m)
 where
  infoDebug = lift . infoDebug

data DebugOpt = NonStop
	      | Step
              | Help

askDebug::IOMonad m => m DebugOpt
askDebug = 
    do { mPutStrLn "\n Debug mode, command? (h = help)"
       ; l <- mGetLine
       ; case parseOpt l of
	  Help -> do { mPutStrLn helpDebug
		     ; askDebug
		     }
          opt  -> return opt
       }

parseOpt "n" = NonStop
parseOpt "s" = Step
parseOpt "h" = Help
parseOpt _   = Step

helpDebug = " (n) - running nonstop \n" ++
            " (s) - step \n" ++
            " (h) - this message \n"  


\end{code}
