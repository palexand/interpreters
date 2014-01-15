\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.UnitTerm where

import InterpreterLib.Algebras
import InterpreterLib.Functors
\end{code}


\begin{code}
data UnitTerm x = UnitTerm

instance Functor UnitTerm where
  fmap f UnitTerm = UnitTerm



instance ZipFunctor UnitTerm where
  zipFunctor f UnitTerm UnitTerm = return UnitTerm

\end{code}


\begin{code}
data UnitTermAlgebra a = UnitTermAlgebra { unitTerm :: AlgSig UnitTerm a }
                              
instance Algebra UnitTerm UnitTermAlgebra a where
  apply alg t = unitTerm alg t



instance AlgebraBuilder UnitTerm (AlgSig UnitTerm a) UnitTermAlgebra a where
  mkAlgebra phi = UnitTermAlgebra phi 
\end{code}


\begin{code}
mkUnit = mkTerm0 UnitTerm

\end{code}