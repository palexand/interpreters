\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.SumTerm(SumTerm(..),
                                    SumTermModule, sumModule) where

import InterpreterLib.Algebras
import InterpreterLib.Functors
import InterpreterLib.Modules
\end{code}


\begin{code}
data SumTerm ty x = SumLeft x ty
                  | SumRight x ty
                  | SumCase x (String, x) (String, x)


instance Functor (SumTerm ty) where
  fmap f (SumLeft x ty) = SumLeft (f x) ty
  fmap f (SumRight x ty) = SumRight (f x) ty
  fmap f (SumCase x (v1,y) (v2,z)) = SumCase (f x) (v1,(f y)) (v2,(f z))


instance ZipFunctor (SumTerm ty) where
  zipFunctor f (SumLeft x ty) (SumLeft y ty') = return (SumLeft (f x y) ty)
  zipFunctor f (SumRight x ty) (SumRight y ty') = return (SumRight (f x y) ty)
  zipFunctor f (SumCase x (v1,y) (v2,z)) (SumCase x' (_,y') (_,z')) = 
      return (SumCase (f x x') (v1, (f y y')) (v2,(f z z')))
  zipFunctor f _ _ = fail "ZipFunctor: Unlike constructors"
\end{code}


\begin{code}
data SumTermAlgebra ty a = SumTermAlgebra { sumLeft :: AlgSig (SumTerm ty) a,
                                            sumRight :: AlgSig (SumTerm ty) a,
                                            sumCase :: AlgSig (SumTerm ty) a
                                       }
instance Algebra (SumTerm ty) (SumTermAlgebra ty) a where
  apply alg t@(SumLeft _ _) = sumLeft alg t
  apply alg t@(SumRight _ _) = sumRight alg t
  apply alg t@(SumCase _ _ _) = sumCase alg t


instance AlgebraBuilder (SumTerm ty) (AlgSig (SumTerm ty) a) (SumTermAlgebra ty) a where
  mkAlgebra phi = SumTermAlgebra phi phi phi
\end{code}


\begin{code}
getMkLeft = mkTerm2 SumLeft
getMkRight = mkTerm2 SumRight
getMkCase = mkTerm3 SumCase



data STMI ty tm = STMI { lc :: tm -> ty -> SumTerm ty tm,
                         rc :: tm -> ty -> SumTerm ty tm,
                         cc :: tm -> (String,tm) -> (String,tm) -> SumTerm ty tm
                        }

data SumTermModule ty tm =  SumTermModule (STMSig ty tm)
            
sumModule = mkModule undefined undefined
  where mkModule (x :: ty) (y :: tm)= let mod :: STMI ty tm
                                          mod = STMI SumLeft SumRight SumCase
                                      in SumTermModule ((mkTerm2 $ lc mod), 
                                                        (mkTerm2 $ rc mod),
                                                        (mkTerm3 $ cc mod))


type STMSig ty tm = (tm -> ty -> tm, 
                     tm -> ty -> tm, 
                     tm -> (String,tm) -> (String,tm) -> tm)

instance Module (SumTermModule ty tm) (STMSig ty tm) where
  open (SumTermModule t) = t
\end{code}