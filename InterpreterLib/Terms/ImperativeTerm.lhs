\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.ImperativeTerm where

import InterpreterLib.Functors
import InterpreterLib.Algebras
\end{code}

\begin{code}
data ImperativeTerm x = NewRef x 
                      | DeRef x
                      | SeqTerm x x


instance Functor ImperativeTerm where
  fmap f (NewRef x) = NewRef (f x)
  fmap f (DeRef x) = DeRef (f x)
  fmap f (SeqTerm x y) = SeqTerm (f x) (f y)

instance ZipFunctor ImperativeTerm where
  zipFunctor f (NewRef x) (NewRef y) = return $ NewRef (f x y)
  zipFunctor f (DeRef x) (DeRef y) = return $ DeRef (f x y)
  zipFunctor f (SeqTerm x y) (SeqTerm u v) = return $ SeqTerm (f x u) (f y v)

\end{code}


\begin{code}

data ImperativeTermAlgebra a = 
    ImperativeTermAlgebra { newRef :: ImperativeTerm a -> a,
                            deRef ::  ImperativeTerm a -> a,
                            seqTerm ::  ImperativeTerm a -> a
                          }



\end{code}


\begin{code}

instance Algebra ImperativeTerm ImperativeTermAlgebra a where
  apply alg t@(NewRef _) = newRef alg t
  apply alg t@(DeRef _) = deRef alg t
  apply alg t@(SeqTerm _ _) = seqTerm alg t


instance AlgebraBuilder ImperativeTerm 
                        (ImperativeTerm a -> a) 
                        ImperativeTermAlgebra a where

  mkAlgebra f = ImperativeTermAlgebra f f f
\end{code}


\begin{code}

mkNewRef = mkTerm NewRef
mkDeRef = mkTerm DeRef
mkSeqTerm = mkTerm2 SeqTerm
\end{code}

