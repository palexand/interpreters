% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Lang] {Functional Language}

This file contains the syntax structure of a functional
language

\begin{comment}
\begin{code}
module LLang
    ( LSyntax
    , R(..)
    , D(..)
    , V(..)
    , CMP(..)
    , U(..)
    , N(..)
    , D(..)
    , TIO(..)
    , P(..)
    , B(..)
    , F(..)
    , Callcc(..)
    ) where
import Basics
import Fix
import SFunctor
import Initial
\end{code}
\end{comment}

\begin{code}

type Struct =
    Sum Callcc -- callcc
  ( Sum D      -- Local Declarations
  ( Sum F      -- Functions
  ( Sum P      -- Trace
  ( Sum V      -- Variables
  ( Sum R      -- References
  ( Sum N      -- Numbers
  ( Sum B      -- Booleans
  ( Sum CMP    -- Comparisons
  ( Sum TIO    -- input-output
        U
  )))))))))

type LSyntax = Fix Struct

instance Initial LSyntax where
 initial = toS (BCons True)

data U x = Undef
           
data N x = Num Integer
         | x `Add` x
         | x `Sub` x
         | x `Mul` x
         | x `Dvd` x
           
data V x = Var Name
           
data B x = BCons Bool
         | x `And` x
         | x `Or` x
         | Cond x x x

data CMP x = x `CLT`  x
           | x `CLE`  x
           | x `CGT`  x
           | x `CGE`  x
           | x `CEQ`  x
           | x `CNEQ` x

data D x = LetV Name x x
         | LetN Name x x
         | LetL Name x x

data F x = LambdaN Name x
         | LambdaV Name x
         | LambdaL Name x
         | App x x

data R x = New x
         | Get x
         | Assign x x
	 | Seq x x
           
data TIO x = Read x
           | Write x
           
data Callcc x = Callcc

data P x = Print   Name
         | ShowMsg String
           
\end{code}

Functor instance declarations (could be done automatically)

\begin{code}

instance Functor U where
  fmap f Undef    = Undef

instance Functor N where
  fmap g (Num n) = Num n
  fmap g (x `Add` y) = g x `Add` g y 
  fmap g (x `Sub` y) = g x `Sub` g y
  fmap g (x `Mul` y) = g x `Mul` g y
  fmap g (x `Dvd` y) = g x `Dvd` g y

instance Functor V 
 where fmap g (Var v) = Var v

instance Functor B where
  fmap f (BCons b)    = BCons b
  fmap f (x `And` y)  = f x `And` f y 
  fmap f (x `Or`  y)  = f x `Or`  f y
  fmap f (Cond x y z) = Cond (f x) (f y) (f z)

instance Functor CMP where
  fmap g (x `CLT` y)  = g x `CLT`  g y 
  fmap g (x `CLE` y)  = g x `CLE`  g y 
  fmap g (x `CGT` y)  = g x `CGT`  g y 
  fmap g (x `CGE` y)  = g x `CGE`  g y 
  fmap g (x `CEQ` y)  = g x `CEQ`  g y 
  fmap g (x `CNEQ` y) = g x `CNEQ` g y 

instance Functor D where
  fmap f (LetV n x y) = LetV n (f x) (f y)
  fmap f (LetN n x y) = LetN n (f x) (f y)
  fmap f (LetL n x y) = LetL n (f x) (f y)

instance Functor F where 	
  fmap f (LambdaN n x) = LambdaN n (f x)
  fmap f (LambdaV n x) = LambdaV n (f x)
  fmap f (LambdaL n x) = LambdaL n (f x)
  fmap f (App x1 x2)   = App (f x1) (f x2)

instance Functor R where
  fmap g (New x)      = New (g x)
  fmap g (Get x)      = Get (g x)
  fmap g (Assign x y) = Assign (g x) (g y)
  fmap g (Seq x y)    = Seq (g x) (g y)

instance Functor TIO where
  fmap g (Read x)      = Read (g x)
  fmap g (Write x)      = Write (g x)

instance Functor Callcc where 
  fmap = cmap

cmap :: (a -> b) -> Callcc a -> Callcc b
cmap f Callcc = Callcc

instance Functor P where 
  fmap f (Print   n) = Print   n
  fmap f (ShowMsg n) = ShowMsg n


\end{code}