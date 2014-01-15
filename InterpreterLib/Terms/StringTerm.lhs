\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.StringTerm where

import InterpreterLib.Algebras
import InterpreterLib.Functors



\end{code}


\begin{code}
data StringTerm a = StringTerm String

instance Functor StringTerm where
  fmap f (StringTerm s)  = (StringTerm s)

instance ZipFunctor StringTerm where
  zipFunctor f (StringTerm x) (StringTerm y) | x == y =  return $ StringTerm x
                                             | otherwise = fail "zipFunctor"


data StringTermAlgebra a = StringTermAlgebra { stringTerm :: StringTerm a -> a }

instance Algebra StringTerm StringTermAlgebra a where
  apply alg t@(StringTerm _) = stringTerm alg t

instance AlgebraBuilder StringTerm (StringTerm a -> a) StringTermAlgebra a where
  mkAlgebra phi = StringTermAlgebra phi


mkString = mkTerm StringTerm


\end{code}
