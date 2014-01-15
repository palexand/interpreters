% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Heap]{Heap definitions}

\begin{comment}
\begin{code}
module Heap ( Heap(..)
	    , getFreeList
	    , setFreeList
	    , allocH
	    , newLocH
	    , getHeap
	    , setHeap
	    ) where
import ErrMonad
import Initial
import Memory
import Basics
import MonadTs
import Runable
\end{code}
\end{comment}

\begin{code}

data Heap val
    = H { freelist :: [Loc]
        , mem      :: Memory val
        }

getFreeList :: Heap v -> [Loc]
getFreeList = freelist

setFreeList :: Heap v -> [Loc] -> Heap v
setFreeList h ls = h { freelist = ls }

getHeap :: (ErrMonad m
	   ) => Heap v -> Loc -> m v
getHeap h l = 
    case getVal (mem h) l of 
     Nothing -> err ("getHeap: cannot get value at loc " 
		    ++ show l)
     Just v  -> return v

setHeap :: (ErrMonad m
	   ) => Heap v -> Loc -> v -> m (Heap v)
setHeap h l v =
    case setVal (mem h) l v of
     Nothing -> err ("setHeap: cannot set value at loc "
		     ++ show l)
     Just m' -> return (h {mem = m'})


newLocH :: (ErrMonad m
	   ) => Heap x -> m (Loc, Heap x)
newLocH heap = 
    case freelist heap of
     (l:ls) -> return (l, heap { freelist = ls })
     []     -> err "freelist is empty"

allocH :: (ErrMonad m
	  ) => v -> Heap v -> m (Loc, Heap v)
allocH v h =
 do { (l,h') <- newLocH h
    ; case setVal (mem h') l v of
	Nothing -> err "allocH: cannot assign value"
        Just m' -> return 
		    (l, h' { mem = m' }) 
    }

instance ( Initial v
         ) => Initial (Heap v) 
 where
  initial = 
      H { freelist = [0..memSize]
	, mem = initMem initial
	}


instance ( Initial v
         , Monad m) => Initial (m v) 
 where
  initial = return initial

instance Show (Heap v) 
 where
  show h = "<<Heap>>"

{-
  "Heap - (freelist = " ++ 
	   show (freelist h) ++ ") " ++
	   " Values = " ++ showMem (mem h)
-}
 
      
\end{code}
