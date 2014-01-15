\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.FixTerm where

import InterpreterLib.Algebras
import InterpreterLib.Functors
\end{code}


\begin{code}
data FixTerm x = FixTerm x

instance Functor FixTerm where
  fmap f (FixTerm x) = FixTerm (f x)



instance ZipFunctor FixTerm where
  zipFunctor f (FixTerm x)  (FixTerm y)  = return (FixTerm (f x y))

\end{code}


\begin{code}
data FixTermAlgebra a = FixTermAlgebra { fixTerm :: AlgSig FixTerm a }
                              
instance Algebra FixTerm FixTermAlgebra a where
  apply alg t = fixTerm alg t



instance AlgebraBuilder FixTerm (AlgSig FixTerm a) FixTermAlgebra a where
  mkAlgebra phi = FixTermAlgebra phi 
\end{code}


\begin{code}
mkFix = mkTerm FixTerm

\end{code}