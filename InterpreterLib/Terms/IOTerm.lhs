\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Terms.IOTerm where

import InterpreterLib.Algebras
import Control.Monad(liftM)
import InterpreterLib.Functors
\end{code}


\begin{code}
data IOTerm a = WriteIO a
              | ReadIO
              

instance Functor IOTerm where
  fmap f (WriteIO x) = WriteIO (f x)
  fmap f ReadIO      = ReadIO

instance ZipFunctor IOTerm where
  zipFunctor f (WriteIO x) (WriteIO y) = return $ WriteIO (f x y)
  zipFunctor f ReadIO ReadIO = return  ReadIO 
  zipFunctor _ _ _ = fail "zipFunctor"

data IOTermAlgebra a = IOTermAlgebra { writeIOTerm :: IOTerm a -> a,
                                       readIOTerm :: IOTerm a -> a
                                     }

instance Algebra IOTerm IOTermAlgebra a where
  apply alg t@(WriteIO x) = writeIOTerm alg t
  apply alg t@ReadIO      = readIOTerm alg t

instance AlgebraBuilder IOTerm (IOTerm a -> a) IOTermAlgebra a where
  mkAlgebra phi = IOTermAlgebra phi phi


mkWrite = mkTerm WriteIO
mkRead = mkTerm0 ReadIO

\end{code}
