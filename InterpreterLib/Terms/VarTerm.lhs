\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.VarTerm where

import InterpreterLib.Algebras
import InterpreterLib.Functors
\end{code}


\begin{code}
type Name = String
data VarTerm a = VarTerm Name | DummyTerm a


instance Functor VarTerm where
  fmap f (VarTerm n) = VarTerm n

instance ZipFunctor VarTerm where
  zipFunctor _ (VarTerm x) (VarTerm y) | x == y = return (VarTerm x) 
                                     | otherwise = fail "Non-matching names"

data VarTermAlgebra a = VarTermAlgebra { varTerm :: VarTerm a -> a
                                       }

instance Algebra VarTerm VarTermAlgebra a where
  apply alg t@(VarTerm _) = varTerm alg t

instance AlgebraBuilder VarTerm (VarTerm a -> a) VarTermAlgebra a where
  mkAlgebra phi = VarTermAlgebra phi

mkVar = mkTerm VarTerm
\end{code}
