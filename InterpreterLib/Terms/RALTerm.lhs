\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.RALTerm where

import InterpreterLib.Algebras
import InterpreterLib.Functors
import InterpreterLib.Terms.VarTerm
\end{code}


\begin{code}
type RegionVar = String
data Place = RegionVar | Deallocated
data RALTerm x = RApp x Place
               | NewRegion RegionVar x
               | RegionAbs RegionVar x
               | At x Place

instance Functor RALTerm where
  fmap f (RApp x place) = RApp (f x) place
  fmap f (NewRegion v x) = NewRegion v (f x)
  fmap f (RegionAbs v x) = RegionAbs v (f x)
  fmap f (At x place) = At (f x) place


instance ZipFunctor RALTerm where
  zipFunctor f (RApp x place) (RApp y _) = return $ RApp (f x y) place
  zipFunctor f (NewRegion v x) (NewRegion _ y) = return $ NewRegion v (f x y)
  zipFunctor f (RegionAbs v x) (RegionAbs _ y) = return $ RegionAbs v (f x y)
  zipFunctor f (At x place) (At y _) = return $ At (f x y) place
\end{code}


\begin{code}
data RALTermAlgebra a = RALTermAlgebra { rApp :: RALTerm a -> a,
                                         newRegion :: RALTerm a -> a,
                                         regionAbs :: RALTerm a -> a,
                                         at :: RALTerm a -> a
                                       }

instance Algebra RALTerm RALTermAlgebra a where
  apply alg t@(RApp _ _) =  rApp alg t
  apply alg t@(NewRegion _ _) = newRegion alg t
  apply alg t@(RegionAbs _ _) = regionAbs alg t
  apply alg t@(At _ _) = at alg t


instance AlgebraBuilder RALTerm (RALTerm a -> a) RALTermAlgebra a where
  mkAlgebra phi = RALTermAlgebra phi phi phi phi
\end{code}

\begin{code}
-- mkRApp t place = inn $ injF $ RApp t place
mkRApp = mkTerm2 RApp
mkNewRegion = mkTerm2 NewRegion
mkRegionAbs = mkTerm2 RegionAbs
mkAt = mkTerm2 At

\end{code}
