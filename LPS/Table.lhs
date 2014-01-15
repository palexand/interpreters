% 
% (c) Jose E. Labra . University of Oviedo (2000)
%

\section[Table] {Lookup Table}

\begin{comment}
\begin{code}
module Table ( Table
	     , addT
	     , lookupT
	     , updateTM
	     , lookupTM
	     , values
	     ) where
import ErrMonad
import Initial
import Basics
import qualified HashTable as H
import Errors
\end{code}
\end{comment}

\begin{code}

type Table x = H.HashTable Name x

instance Initial (Table x) 
 where
  initial = H.emptyTable

addT :: Name -> v -> 
	  Table v -> Table v
addT k v t = H.addKey k v t

lookupT :: ErrMonad m => 
	  Name -> Table v -> m v
lookupT k t = 
    case H.lookupKey k t of 
     Nothing -> errUnbound k
     Just v  -> return v
     

updateTM :: (ErrMonad m) => 
	   (Name, m v) -> 
	       Table (m v) -> 
		   m (Table (m v))  
updateTM (k, v) t = 
    return $ H.addKey k v t

lookupTM :: (ErrMonad m) 
	   => Name -> 
	       Table (m v) -> m v
lookupTM k t = 
    case H.lookupKey k t of
     Just m  -> m 
     Nothing -> errUnbound k

values :: Table v -> [(Name,v)]
values = H.values

{- instance Show (Table x) 
 where
  show (Table t) = "<<Table>>" -}

\end{code}
