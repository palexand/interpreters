\section{Functors, Algebras and Catamorphsisms}

\begin{code}
  module Cata where
\end{code}

The catamorphism and fold depend on the definition of the standard
datatype least fixpoint.  |Fix| type defines a template for fixed
point data tyeps.  Note that the |In| constructor is required by
\texttt{Haskell} to create instances of |Fix|.  The |out| function is
effectively the opposite of the |In| constructor and pulls the
encapsulated data structure out of the constructor.

\begin{code}

  newtype Fix f = In (f (Fix f))

  out :: Fix f -> f (Fix f)
  out (In x) = x

\end{code}

Now we add the Algebra and Co-Algebraic notation. The original type
for fold:

\begin{spec}
  fold :: (Functor f) => (f a -> a) -> Fix f  -> a
\end{spec}

becomes:

\begin{spec}
  fold :: (Functor f) => (Algebra f a) -> Fix f -> a
\end{spec}

using the type classes |Functor| and |Algebra|.  Even simpler, we can
use the (Co)AlgebraConstructor class:

\begin{spec}
  pCata :: (AlgebraConstructor f a) => Fix f -> a
  pCata = phi . fmap pCata . out
\end{spec}

Informally, we can define |pCata| as:

\begin{spec}
  pCata == polytypic Catamorphism
\end{spec}

Note the strong similarity between the polymorphic Catamorphism and
the polymorphic fold, just above.  |fold| \emph{asks for} function
|phi (F a -> a)| as an argument while |pCata| \emph{knows} |phi|.

We now take these concepts and encode them as \texttt{Haskell} type
classes and types.  First, define |Algebra|, |CoAlgebra|,
|AlgebraConstructor| and |CoAlgebraConstructor|:

\begin{code}

  type Algebra   f a  = f a -> a
  type CoAlgebra f a  = a   -> f a 

  class Functor f => AlgebraConstructor f a
      where phi :: Algebra f a

  class Functor f => CoAlgebraConstructor f a
      where psi :: CoAlgebra f a

\end{code}

We now redefine |fold| using |Functor| and |Algebra|.  This is very
similar to Duponcheel.

\begin{code}

  -- fold :: (Functor f) => (f a -> a) -> Fix f -> a
  fold :: (Functor f) => (Algebra f a) -> Fix f -> a
  fold g = g . fmap (fold g) . out

\end{code}

We can similarly define |pCata| using the |AlgebraConstructor| type class:

\begin{code}

  pCata :: (AlgebraConstructor f a) => Fix f -> a
  pCata = phi . fmap pCata . out

\end{code}

Finally, we re-define the denotational semantics for the simple
expression language that we began with in \texttt{FoldUnfold.lhs}
using, |Algebra|, datatype fixpoint and polytypic fold.  Note that it
is a little more complex because we carefully maintained generality
over the specific numeric type used for constant values. The type,
|E|, requires two type arguments, and everywhere we have to add the
constraint |(Num a)|.

\begin{code}

  data (Num numType) => E numType e = Val numType
                                    | Add e e

  type Expr numType = Fix (E numType)

  instance (Num a) =>  Functor (E a) where
      fmap f (Val n) = Val n
      fmap f (Add e1 e2) = Add (f e1) (f e2)

-- combine :: (Num a) => (E a) a -> a
  combine :: (Num a) => Algebra (E a) a
  combine (Val n) = n
  combine (Add n1 n2) = (n1 + n2)

  instance (Num a) =>(AlgebraConstructor (E a) a) where
      phi = combine

  evalExpr :: (Num a) => (Expr a) -> a
  evalExpr = fold combine

\end{code}

We can use |evalExprCata| to get the same result with much less work:

\begin{code}

  evalExprCata :: (Num a) => (Expr a) -> a
  evalExprCata = pCata

\end{code}

|test1| is a sample expression for evaluation: $(3 + ((1 + 10) + 4))$

\begin{code}
  test1 = (In (Add (In (Val 3))
	       (In (Add (In (Add (In (Val 1))
			     (In (Val 10))))
		    (In (Val 4))))))
\end{code}

To combine with Duponcheel, we would like to compose our semantic
evaluation functions into a single function.  For this, we will use a
|Sum| type defined exactly as in Duponcheel:

\begin{code}
  newtype Sum f g x = S (Either (f x) (g x))

  unS (S x) = x
\end{code}

and show that |Sum| types are members of the |Functor| and
|AlgebraConstructor| classes so we can use |pCata| as our evaluation
function.

\begin{code}

  instance (Functor f, Functor g) => Functor (Sum f g) 
      where fmap h  (S (Left x))  = S (Left  $ fmap h x)
	    fmap h  (S (Right x)) = S (Right $ fmap h x)

\end{code}

Here, using catamorphism instead of just fold, is a big win.
the definition:

\begin{spec}
  phi = either phi phi . unS
\end{spec}

uses |phi| to refer to 3 different implementations |phi|.  We define
|phi| for the |Sum| type, using the |phi| of each of the types we are
|Sum|'ing.  This is the result of using the |Algebra| type class
synonym rather than defining a separately named |phi| for each term.

\begin{code}

  instance (AlgebraConstructor f a, AlgebraConstructor g a) => 
      AlgebraConstructor (Sum f g) a 
--     phi :: Algebra fa == f a -> a
	  where phi = either phi phi . unS

\end{code}

As an example we extend Expressions defined above to include
multiplication.  I use this simple extension for brevity.  For
\emph{any} extension, all we need to do is:

\begin{enumerate}
  \parskip=0pt\itemsep=0pt
\item Define a new type for the extension
\item Show the extension is belongs to Functor and AlgebraConstructor
\item Define a new type, using Sum, to combine the original type with
  the extension type.
\end{enumerate}

And, we have an evaluator for the more complex type, without modifying
the base type and without defining any interaction between the two
types.

\begin{code}

  data (Num numType) => M numType e =  Times e e

  instance (Num a) =>  Functor (M a) where
      fmap f (Times e1 e2) = Times (f e1) (f e2)

  instance (Num a) => (AlgebraConstructor (M a) a) where
      phi (Times x y) = x * y

  data BigE n a = BE (Sum (E n) (M n) a)

  type BigExpr numType = Fix (BigE numType)

\end{code}

Just a bit of boiler plate stuff.  Since \texttt{Haskell} requires
that the new type, |BigE|, use a data constructor (named, |BE|, here);
we have to show that this type is a member of |Functor| and
|AlgebraConstructor| by simply ripping off this data constructor.

\begin{code}

  instance (Num a) => Functor (BigE a) where
      fmap f (BE x) = BE (fmap f x)

  instance (Num a) => (AlgebraConstructor (BigE a) a) where
      phi (BE x) = phi x

  evalBigExpr :: (Num a) => (BigExpr a) -> a
  evalBigExpr = pCata 

\end{code}

We can do the same thing with fold, but is a bit more clumsy.
Now we must explicitly define the function to fold'ed over the sum.

The class |AlgebraConstructor| (and then catamorphism) are able to
eliminate this step, since this function is already known,
with the name |phi|.

\begin{code}

  unBE (BE x) = x

  combineM :: (Num a) => (M a) a -> a
  combineM (Times x y) = x * y

  summedCombine :: (Num a) => (BigE a) a -> a
  summedCombine = either combine combineM . unS . unBE

  evalBigExprFold  :: (Num a) => (BigExpr a) -> a
  evalBigExprFold = fold summedCombine

\end{code}
