\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.ProcTerm where

import InterpreterLib.Algebras
import InterpreterLib.Functors

\end{code}


\begin{code}
type Name = String
data ProcTerm x = Procedure Name [Name] x |
                  ProcCall x [x]

newtype ProcValue v = ProcValue ([v] -> v)

instance Functor ProcTerm where
  fmap f (Procedure procname ns body) = Procedure procname ns (f body)
  fmap f (ProcCall fun args) = ProcCall (f fun) (map f args)


instance ZipFunctor ProcTerm where
  zipFunctor f (Procedure m ms b1) (Procedure n ns b2) 
      | m == n && ms == ns = return $ Procedure m ms (f b1 b2)
      | otherwise = fail "zipFunctor"
  zipFunctor f (ProcCall f1 a1) (ProcCall f2 a2) = 
      return $ ProcCall (f f1 f2) (zipWith f a1 a2)

data ProcTermAlgebra a = ProcTermAlgebra { procTerm :: ProcTerm a -> a,
                                           callTerm :: ProcTerm a -> a
                                         }

instance Algebra ProcTerm ProcTermAlgebra a where
  apply alg t@(Procedure _ _ _) = (procTerm alg) t
  apply alg t@(ProcCall _ _) = (callTerm alg) t

instance AlgebraBuilder ProcTerm (ProcTerm a -> a) ProcTermAlgebra a where
  mkAlgebra f = ProcTermAlgebra f f



mkProc = mkTerm3 Procedure
mkCall = mkTerm2 ProcCall

\end{code}
