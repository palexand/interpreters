% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[SemMonad] {Common Semantic definitions}

\begin{comment}
\begin{code}
module SemMonad( BinOp
	       , evalAndApply
	       , prjAndApply
	       , applyOp
	       , evalOperands
	       , prjOperands
	       , checkType
	       , returnInjF
	       , mInfo
	       , module ErrMonad
	       , module EnvMonad
	       , module StateMonad
	       , module IOMonad
	       , module ContMonad
	       , module DebugMonad
	       , module HasName
	       , module SubType
	       , module Basics
	       , module Initial
) where
import HasName
import ErrMonad
import StateMonad
import EnvMonad
import IOMonad
import ContMonad
import DebugMonad
import Basics
import SubType
import Initial
\end{code}
\end{comment}

\begin{code}
type BinOp t1 t2 = t1 -> t1 -> t2

evalAndApply :: (HasName v, 
          	 ErrMonad m, 
          	 SubType t1 v, 
          	 SubType t2 v) => BinOp t1 t2 -> m v -> m v -> m v
evalAndApply op mx my = do{ (x,y)    <- evalOperands mx my;
			    prjAndApply op x y;
                          } 

prjAndApply :: (HasName v, 
          	ErrMonad m, 
          	SubType t1 v, 
          	SubType t2 v) => BinOp t1 t2 -> v -> v -> m v
prjAndApply op x y = do{ (px, py) <- prjOperands x y;
                         applyOp op px py
                       } 

applyOp:: (HasName v, 
           ErrMonad m, 
           SubType t1 v, 
           SubType t2 v) => BinOp t1 t2 -> t1 -> t1 -> m v
applyOp op x y = returnInj (op x y)

evalOperands::(Monad m) => m v -> m v -> m (v, v) 
evalOperands mx my = do { x <- mx; 
                          y <- my; 
                          return (x,y)
                        }

prjOperands::(ErrMonad m, 
	      HasName v,	
              SubType t v) => v -> v -> m (t, t) 
prjOperands x y = do { px <- checkType x; py <- checkType y;
                       return (px,py)
                     }

checkType:: ( ErrMonad m
	    , HasName a
	    , SubType b a) 
           => a -> m b
checkType x =  maybe (err ("Type error: expected type " ++ 
			   name x)) 
	              return (prj x) 

returnInjF :: ( Monad m
	      , SubType (m v -> m v) v) 
	     => (m v -> m v) -> m v
returnInjF = returnInj

			

mInfo :: ( IOMonad m
	 , Show v
	 ) => Name -> v -> v -> m ()
mInfo var loc value = 
    mPutStrLn (   "var " ++ var 
	       ++ ", loc = " ++ show loc
	       ++ ", value = " ++ show value
	      )

\end{code}