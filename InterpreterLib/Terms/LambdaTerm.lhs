\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.LambdaTerm(LambdaTerm(..), 
                                       LambdaTermAlgebra(..),
                                       LambdaTermModule, lambdaModule) where

import InterpreterLib.Algebras
import InterpreterLib.Functors
import InterpreterLib.Modules
\end{code}


\begin{code}
data LambdaTerm ty x = App x x
                     | Lam String ty x 

instance Functor (LambdaTerm ty) where
  fmap f (App x y) = App (f x) (f y)
  fmap f (Lam s ty x) = Lam s ty (f x)

instance ZipFunctor (LambdaTerm ty) where
  zipFunctor f (App a b) (App x y) = return $ App (f a x) (f b y)
  zipFunctor f (Lam n ty x) (Lam _ _ y) = return $ Lam n ty (f x y)


data LambdaTermAlgebra ty a = LambdaTermAlgebra { app :: LambdaTerm ty a -> a,
                                                  lam :: LambdaTerm ty a -> a
                                                }

instance Algebra (LambdaTerm ty) (LambdaTermAlgebra ty) a where
  apply alg t@(App _ _) = app alg t
  apply alg t@(Lam _ _ _) = lam alg t

instance AlgebraBuilder (LambdaTerm ty) (LambdaTerm ty a -> a) (LambdaTermAlgebra ty) a where
  mkAlgebra f = LambdaTermAlgebra f f 
\end{code}


\begin{code}
{-
data LambdaTermModule ty tm =
  LambdaTermModule  { lambdaConstructor :: String -> ty -> tm -> LambdaTerm ty tm,
                      appConstructor ::  tm -> tm -> LambdaTerm ty tm
                    }
-}

data LTMI ty tm =
  LTMI  { lc :: String -> ty -> tm -> LambdaTerm ty tm,
          ac ::  tm -> tm -> LambdaTerm ty tm
        }


data LambdaTermModule ty tm = 
  LambdaTermModule  { getMkLambda :: String -> ty -> tm -> tm,
                      getMkApp ::  tm -> tm -> tm
                    }


mkModule (x :: ty) (y :: tm)= let mod :: LTMI ty tm
                                  mod = LTMI Lam App
                              in LambdaTermModule (mkTerm3 $ lc mod) (mkTerm2 $ ac mod)

lambdaModule = mkModule undefined undefined



instance Module (LambdaTermModule ty tm) (String -> ty -> tm -> tm, 
                                          tm -> tm -> tm) where
  open (LambdaTermModule l a) = (l,a)


\end{code}