\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.RecordTerm where

import InterpreterLib.Algebras
import InterpreterLib.Functors
\end{code}


\begin{code}
data RecordTerm x = RecordTerm [x]
                  | ProjTerm x Int


instance Functor RecordTerm where
  fmap f (RecordTerm fields) = RecordTerm (fmap f fields)
  fmap f (ProjTerm x field) = ProjTerm (f x) field

instance ZipFunctor RecordTerm where
  zipFunctor f (RecordTerm fields) (RecordTerm fields') = 
      return (RecordTerm (zipWith f fields fields'))
  zipFunctor f (ProjTerm x l) (ProjTerm y l') | l == l' = return (ProjTerm (f x y) l)
                                      | otherwise = fail "Field labels don't match"
  zipFunctor f _ _ = fail "ZipFunctor: Unlike constructors"
\end{code}


\begin{code}

data RecordTermAlgebra a = RecordTermAlgebra { recordTerm :: AlgSig RecordTerm a,
                                               projTerm :: AlgSig RecordTerm a }


instance Algebra RecordTerm RecordTermAlgebra a where
  apply alg t@(RecordTerm _) = recordTerm alg t
  apply alg t@(ProjTerm _ _) = projTerm alg t


instance AlgebraBuilder RecordTerm (AlgSig RecordTerm a) RecordTermAlgebra a where
  mkAlgebra phi = RecordTermAlgebra phi phi
\end{code}


\begin{code}
mkRecord = mkTerm RecordTerm
mkProj = mkTerm2 ProjTerm
\end{code}