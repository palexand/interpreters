\begin{code}
{-# OPTIONS -fglasgow-exts -fno-monomorphism-restriction #-}
module InterpreterLib.Algebras where

import InterpreterLib.Functors
import Monad(liftM, liftM2)
import List((\\))
import InterpreterLib.SubType

infixr 5 @+@
\end{code}


One of the problems that we have with algebras in Duponcheel's and
Gayo's work is that they are overloaded functions, meaning only one
algebra is possible for a given functor/value space pair without
giving us overlapping instances. It would be nice to be able to pass
the algebra as a first class value, as well as do things like algebra
extension, where one algebra extends the other.

Essentially an algebra is a parameterized function with a type |F a ->
a| for a given functor |F| and a \emph{carrier set} |a|. There are
some Haskell reasons we want to make the actual type of algebras
abstract, instead just |type AType f a = f a -> a|, because we may
want to manipulate algebras when combining two of them, and functions
are opaque. A more transparent form, such as the record format we use
below, gives us more flexibility for doing these
manipulations. Unfortunately, we have to have the functional
dependency |f -> alg| ("the term type |f| uniquely determines the
algebra representation |alg|"). Note that this means the algebra
representation - the packaging around the function |f a -> a|, and not
the actual function itself is uniquely. The problem with this is that
we can't just combine two different algebra representations or even
have them visible in the same namespace. One research question is how
to combine heterogeneous algebra representations. One possibility is
to use the |Arrows| abstraction of John Hughes.

6/12 - updated functional dependencies so that the dependency is
reflexive, allowing the pairAlg stuff to work.

\begin{code}
type AlgSig f a = f a -> a
\end{code}

Define a different |Algebra| than that used in standard approaches.
This |Algebra| is parameterized over the traditional carrier set and
an algebra structure.  The algebra structure is the data type that
provides definitions for the |Algebra|.  Functional dependencies state
that f can be uniquely determined from alg.  As is typical, for something to be and |Algebra| it must also be a |Functor|.

\begin{code}
class Functor f => Algebra f alg a | alg -> f where
  apply :: alg a -> f a -> a
\end{code}

Algebra builder seems to define a mechanism for transforming some type
into an algebra.  I'm guessing this is for defining standard algebra
formers

\begin{code}
class Algebra f alg a => AlgebraBuilder f fType alg a | fType -> f, fType -> a, fType -> alg where
  mkAlgebra :: fType -> (alg a)
\end{code}  

Parameterize |cata| over |Algebra|.  Nice.  |(apply alg)| effectively
produces the function |phi| from the |alg| structure.  Remember, |alg|
is an algebra structure that contains functions defined for an
|Algebra|.  When used in this manner with |apply|, the appropriate
function is extracted from the algebra and applied.

\begin{code}
cata alg = (apply alg) . (fmap (cata alg)) . out
\end{code}

|pairAlg| is a mechanism for pairing algebras.

\begin{code}
pairAlg a1 a2 = mkAlgebra pa
  where pa term = (((apply a1) . (fmap fst)) term, ((apply a2) . (fmap snd)) term)
\end{code}

Define sums of algebras in the same manner as sums of terms.  Note
that the same functions are defined, but with different argument
types.  The first definition seems to be a spurrious example and
should be ignored for the time being.

\begin{code}
sumAlg a1 a2 = SumAlgebra {left = sumAlg',
                           right = sumAlg'}
  where sumAlg' (S (Left x)) = apply a1 x
        sumAlg' (S (Right x)) = apply a2 x

(@+@) = sumAlg

data SumAlgebra f g a = SumAlgebra {left :: Sum f g a -> a,
                                    right :: Sum f g a -> a}


instance (Functor f, Functor g) => Algebra (Sum f g) (SumAlgebra f g) a where
  apply alg t@(S (Left _)) = left alg t
  apply alg t@(S (Right _)) = right alg t
  

instance (Functor f, Functor g) => 
  AlgebraBuilder (Sum f g) (Sum f g a -> a) (SumAlgebra f g) a where
  mkAlgebra f = SumAlgebra f f
\end{code}

