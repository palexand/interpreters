% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Fix] {Fixpoint definitions}

\begin{comment}
\begin{code}
module Fix (Fix, inF, outF, 
            ana, cata, 
            hylo,
            pAna, pCata,
            Algebra, CoAlgebra,
            AlgebraC(..), CoAlgebraC(..),
            fixPoint)
where
\end{code}
\end{comment}

|Fix| allows the definition of recursive datatypes as least 
        fixpoints of non-recursively defined functors

\begin{code}

newtype 
  -- (Functor f) => this context generates an internal error in GHC  
  Fix f      = In { outF :: f (Fix f) } 

inF :: (Functor f) => f (Fix f) -> Fix f
inF = In

\end{code}

f-Algebras and f-CoAlgebras. 

\begin{code}
type Algebra f a   = f a -> a
type CoAlgebra f a = a -> f a 

class Functor f => AlgebraC f a   where phi :: Algebra f a
class Functor f => CoAlgebraC f a where psi :: CoAlgebra f a
\end{code}

polymorphic {cata-ana} morphism 

\begin{code}
pCata::(AlgebraC f a) => Fix f -> a
pCata = phi . fmap pCata . outF

pAna::(CoAlgebraC f a) => a -> Fix f
pAna  = In . fmap pAna  . psi

\end{code}

The following definition of polymorphic hylomorphism gives an
ambiguous type error.  

\begin{code}
-- pHylo::(AlgebraC f a, CoAlgebraC f b) => b -> a
-- pHylo = cata . ana
\end{code}

Classical {cata-ana-hylo-para} morphisms

\begin{code}
cata::(Functor f) => 
      Algebra f a -> 
          Fix f -> a
cata phi = phi . fmap (cata phi) . outF

ana::(Functor f) => 
     CoAlgebra f a -> 
         a -> Fix f
ana  psi = In . fmap (ana psi) . psi

hylo::(Functor f) => 
      Algebra f a -> 
          CoAlgebra f b -> 
              b -> a
hylo phi psi = cata phi . ana psi   

para::(Functor f)=>(f a -> Fix f -> a) -> Fix f -> a
para g x = g (fmap (para g) (outF x)) x

\end{code}


\begin{code}
instance AlgebraC f ShowS => Show (Fix f) where
 showsPrec p = pCata
\end{code}

The next definition looks for the fixpoint of a function

\begin{code}
fixPoint::Eq a =>
          (a -> a) -> 
              a -> a
fixPoint f x | fx == x   = x
             | otherwise = fixPoint f fx
  where fx = f x
\end{code}
