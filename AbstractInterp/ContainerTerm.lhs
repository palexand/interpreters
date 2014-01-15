This module is experimental and subject to change without notice.  Use at your own risk or keep a local copy!

\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.ContainerTerm where
import InterpreterLib.Algebras
import InterpreterLib.Functors
import InterpreterLib.Modules
\end{code}

We really don't need definitions for equal below since we have
sub-container functions.  All of them are equal if sub-container holds
both ways.  We do need the inclusion term because of the different ways
structures handle ordering.

\begin{code}
data ContainerTerm ty x
    = Set ty [x]
    | Bag ty [x]
    | Seq ty [x]
    | InTerm x x
    | SubTerm x x
    | EqTerm x x
      deriving (Show,Eq)

instance Functor (ContainerTerm ty) where
  fmap f (Set ty s) = (Set ty (map f s))
  fmap f (Bag ty s) = (Bag ty (map f s))
  fmap f (Seq ty s) = (Seq ty (map f s))
  fmap f (InTerm t1 t2) = (InTerm (f t1) (f t2))
  fmap f (SubTerm t1 t2) = (SubTerm (f t1) (f t2))
  fmap f (EqTerm t1 t2) = (EqTerm (f t1) (f t2))

--instance ZipFunctor (ContainerTerm ty) where
--  zipFunctor g (Set ty x) (Set _ y)
--      = return $ (Set ty (g x y))
--  zipFunctor g (Bag ty x) (Bag _ y)
--      = return $ (Bag ty (g x y))
--  zipFunctor g (Seq ty x) (Seq _ y)
--      = return $ (Seq ty (g x y))


data ContainerTermAlgebra ty a
    = ContainerTermAlgebra { tset :: ContainerTerm ty a -> a,
                             tbag :: ContainerTerm ty a -> a,
                             tseq :: ContainerTerm ty a -> a,
                             tin :: ContainerTerm ty a -> a,
                             tsub :: ContainerTerm ty a -> a,
                             teq :: ContainerTerm ty a -> a
                           }

instance Algebra (ContainerTerm ty) (ContainerTermAlgebra ty) a where
  apply alg t@(Set _ _) = tset alg t
  apply alg t@(Bag _ _ ) = tbag alg t
  apply alg t@(Seq _ _) = tseq alg t
  apply alg t@(InTerm _ _) = tin alg t
  apply alg t@(SubTerm _ _) = tsub alg t
  apply alg t@(EqTerm _ _) = teq alg t

instance AlgebraBuilder (ContainerTerm ty) (ContainerTerm ty a -> a) (ContainerTermAlgebra ty) a where
  mkAlgebra f = ContainerTermAlgebra f f f f f f
\end{code}


