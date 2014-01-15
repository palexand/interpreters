\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.IfTerm where

import InterpreterLib.Algebras
import InterpreterLib.Functors
\end{code}


\begin{code}
data IfTerm a = IfTerm a a a
              | TrueTerm
              | FalseTerm

instance Functor IfTerm where
  fmap f (IfTerm x y z) = IfTerm (f x) (f y) (f z)
  fmap f TrueTerm   = TrueTerm
  fmap f FalseTerm  = FalseTerm

instance ZipFunctor IfTerm where
  zipFunctor f (IfTerm a b c) (IfTerm x y z) = return $ IfTerm (f a x) (f b y) (f c z)
  zipFunctor f TrueTerm TrueTerm = return TrueTerm
  zipFunctor f FalseTerm FalseTerm = return FalseTerm
  zipFunctor f _ _ = fail "ZipFunctor: Unlike constructors"
\end{code}

\begin{code}
data IfTermAlgebra a = IfTermAlgebra { ifTerm :: IfTerm a -> a,
                                       trueTerm :: IfTerm a -> a,
                                       falseTerm :: IfTerm a -> a
                                     }

instance Algebra IfTerm IfTermAlgebra a where
  apply alg t@(IfTerm _ _ _) = ifTerm alg t
  apply alg t@TrueTerm       = trueTerm alg t
  apply alg t@FalseTerm      = falseTerm alg t

instance AlgebraBuilder IfTerm (IfTerm a -> a) IfTermAlgebra a where
  mkAlgebra phi = IfTermAlgebra phi phi phi


mkIf = mkTerm3 IfTerm
mkTrue = mkTerm0 TrueTerm
mkFalse = mkTerm0 FalseTerm
\end{code}
