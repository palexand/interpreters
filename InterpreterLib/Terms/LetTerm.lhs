\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.LetTerm where

import InterpreterLib.Algebras
import InterpreterLib.Functors


\end{code}


\begin{code}
type Name = String
data LetTerm ty a = LetTerm [(Name, ty, a)] a
                  | LetRecTerm [(Name, ty, a)] a


instance Functor (LetTerm ty) where
  fmap f (LetTerm bindings body) = LetTerm (map (\(n,ty,v) -> (n,ty,f v)) bindings) (f body)
  fmap f (LetRecTerm bindings body) = LetRecTerm (map (\(n,ty,v) -> (n,ty,f v)) bindings) (f body)


instance ZipFunctor (LetTerm ty) where
  zipFunctor f (LetTerm bs1 b1) (LetTerm bs2 b2) = 
      return $ LetTerm (zipWith fun bs1 bs2) (f b1 b2)
    where fun (n1,ty,v1) (n2,_,v2) = (n1,ty, (f v1 v2))

  zipFunctor f (LetRecTerm bs1 b1) (LetRecTerm bs2 b2) = 
      return $ LetRecTerm (zipWith fun bs1 bs2) (f b1 b2)
    where fun (n1,ty,v1) (n2,_,v2) = (n1, ty,(f v1 v2))

data LetTermAlgebra ty a = LetTermAlgebra { letTerm :: AlgSig (LetTerm ty) a,
                                            letRecTerm :: AlgSig (LetTerm ty) a
                                          }

instance Algebra (LetTerm ty) (LetTermAlgebra ty) a where
  apply alg t@(LetTerm _ _) = letTerm alg t
  apply alg t@(LetRecTerm _ _) = letRecTerm alg t

instance AlgebraBuilder (LetTerm ty) (LetTerm ty a -> a) (LetTermAlgebra ty) a where
  mkAlgebra phi = LetTermAlgebra phi phi

mkLet = mkTerm2 LetTerm
mkLetRec = mkTerm2 LetRecTerm

\end{code}
