% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[MFix] {Monadic Fixpoint definitions}

\begin{comment}
\begin{code}
module MFix where
import Fix

infixl 5 @@
\end{code}
\end{comment}

Monadic functions and monadic composition: 

\begin{code}
type -- (Monad m) => Haskell doesn't allow contexts in type synonyms
     MFun m a b = a -> m b
\end{code}

A different way to define Monadic functions could be:
\begin{code}
newtype (Monad m) => MFunction m a b = MF (a -> m b)
\end{code}

Monadic function composition:

\begin{code}
(@@)::(Monad m) => (MFun m b c) -> (MFun m a b) -> (MFun m a c)
f @@ g = \x -> g x >>= f

\end{code}

Monadic F-Algebra

\begin{code}
type MAlgebra   m f a = f a -> m a
type MCoAlgebra m f a = a -> m (f a)

class (Functor f) => MFunctor f where
  mfmap::(Monad m) => (a -> m b) -> (f a -> m (f b))

mIn ::(Monad m, Functor f) => (f (Fix f)) -> m (Fix f)
mIn = return . inF

mOut::(Monad m, Functor f) => Fix f -> m (f (Fix f))
mOut = return . outF

\end{code}

Monadic {cata-ana-hylo} morphism

\begin{code}
mCata::( Monad m
       , MFunctor f) 
       => MAlgebra m f a -> Fix f -> m a
mCata phi = phi @@ mfmap (mCata phi) @@ mOut

mAna::( Monad m
      , MFunctor f) 
      => MCoAlgebra m f a -> a -> m (Fix f)
mAna psi = mIn @@ mfmap (mAna psi) @@ psi

mHylo::( Monad m
       , MFunctor f) => 
       MAlgebra m f a -> 
           MCoAlgebra m f b -> (b -> m a)
mHylo phi psi = mCata phi @@ mAna psi

-- Monadic paramorphisms (?)
-- mPara::(Monad m, MFunctor m f)=>(f a -> Fix f -> m a) -> Fix f -> m a
-- mPara g x = g (mfmap (mPara g) (mOut x)) x

\end{code}

\begin{code}
class ( Monad m  
      , MFunctor f) 
    => MAlgebraC   m f a 
  where mPhi :: f a -> m a

class ( Monad m
      , MFunctor f) 
    => MCoAlgebraC m f a 
  where mPsi :: a -> m (f a)

\end{code}

Polymorphic monadic {ana-cata} morphism

\begin{code} 
pMCata::(MAlgebraC m f a) => MFun m (Fix f) a
pMCata = mPhi @@ mfmap pMCata @@ mOut

pMAna::(MCoAlgebraC m f a) => MFun m a (Fix f)
pMAna  = mIn @@ mfmap pMAna @@ mPsi
\end{code}