% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[IOT] {Input-Output monad transformer}

\begin{comment}
\begin{code}
module IOT where
import Basics
import Initial
import MonadT
import ShowMonad
import ShowUtils
import StateT
import IOMonad
import IOExts
\end{code}
\end{comment}

Input-Output can be modelled as a specialised 
State transformer where the state contains
the input and output channels

\begin{code}

newtype IOT m v = 
    IOT (IOChannels -> 
	         m (v, IOChannels) )

unIOT (IOT m) = m

data IOChannels = 
       IOC { input :: String
	   , output :: String
	   } 

instance Show IOChannels 
 where
   show a = "<<IO Channels>>"


instance ( Monad m 
	 ) => Monad (IOT m) 
 where
   return v = IOT (\cs -> return (v,cs))
   (>>=) m f = 
       IOT (\cs -> 
	      do { (v,cs') <- unIOT m cs
		 ; unIOT (f v) cs'
	         }
	   )

instance ( Monad m
	 , Monad (IOT m))
    => MonadT IOT m 
 where
   lift m = 
       IOT (\cs -> do { x <- m
		      ; return (x,cs)
		      })  

instance ( Monad m
	 ) => IOMonad (IOT m) 
 where
   put c = 
     IOT (\cs -> return ( (), putInChannels c cs))

   get = 
     IOT (return . getInChannels)


putInChannels :: Char -> IOChannels -> IOChannels
putInChannels c cs = cs { output = c:output cs}

getInChannels :: IOChannels -> (Char,IOChannels)
getInChannels cs = 
    case input cs of
      (b:bs) -> (b, cs { input = bs})
      []     -> error "getInChannels:: Empty input"

initChannels :: String -> IOChannels
initChannels s = 
    IOC { input = s
	, output = [] 
	}

instance ( IOMonad m
	 , MonadT t m
	 ) => IOMonad (t m) 
 where
   put = lift . put
   get = lift get


\end{code}