\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module InterpreterLib.Functors where
infixr 5 :$:
\end{code}

Define the standard fixed point for types.  The constructor |In| is
necessary to keep Haskell happy during type checking.

\begin{code}
newtype Fix f = In (f (Fix f))
inn = In
out (In x) = x
\end{code}

Define the standard |Sum| type by encapsulating |Either|.  Again, |S|
is around to keep Haskell happy during type checking.  Note the
definition of an infix constructor alias.

\begin{code}
newtype Sum f g x = S (Either (f x) (g x))
unS (S x) = x
type x :$: y = Sum x y 
\end{code}

Define a sum of functors to be a functor by defining fmap
appropriately.  If we have two functors, a new functor can be defined
from the |Sum| by using |Left| and |Right| to guide the application of
|fmap|.  Not much to it really, just stripping away the constructor,
applying the |fmap| to the carried data, and putting the constructor
back together.

\begin{code}
instance (Functor f, Functor g) => Functor (Sum f g) 
 where fmap h  (S (Left x))  = S (Left  $ fmap h x)
       fmap h  (S (Right x)) = S (Right $ fmap h x)
\end{code}

Define |SubFunctor| in a manner very similar to the way |SubSum|.
|injF| injects the subfunctor into its super functor.  |prjF| does the
opposite if it can.  Note the use of the |Maybe| type.

\begin{code}
class SubFunctor f g where 
    injF :: f x -> g x
    prjF :: g x -> Maybe (f x)
\end{code}

An |Functor| is a |SubFunctor| of itself.

\begin{code}
instance SubFunctor f f where 
    injF f = f
    prjF = Just
\end{code}

Any |Functor| is a |SubFunctor| of a |Sum| that includes it as the
left side of the pair.

\begin{code}
instance SubFunctor f (Sum f x) where
    injF               = S . Left
    prjF (S (Left f))  = Just f
    prjF (S (Right x)) = Nothing
\end{code}

If |f| is a |SubFunctor| of |g|, then it is also a |SubFunctor| of the
|Sum| of any |x| and |g|.

\begin{code}
instance ( SubFunctor f g) => SubFunctor f (Sum x g) where
    injF               = S . Right . injF
    prjF (S (Left x))  = Nothing
    prjF (S (Right b)) = prjF b
\end{code}

Remaining functions seem to be helper functions of various kinds.
|toS| seems to inject a term defined over a fixed point into a fixed
point.  The various |mkTerm| functions are used to create terms of
various arities.

\begin{code}
toS :: (SubFunctor f g, Functor g) => f (Fix g) -> Fix g
toS x = inn $ injF x

mkTerm0 = toS
mkTerm  f = toS . f
mkTerm2 f = curry $ toS . (uncurry f)
mkTerm3 f = curry $ curry $ toS . (uncurry (uncurry f))
\end{code}

|ZipFunctor| provides a function that performs the standard |Functor|
operation, but over two inputs rather than one.  Remember that Functor
takes a function, |(a -> b)|, and a term defined using a parameterized
type, |f a|, and changes the term to use the carrier set |b| rather
than |a|.  |ZipFunctor| does the same thing, but the input langauge
parameterized over two types.

\begin{code}
class ZipFunctor f where
  zipFunctor :: Monad m => (a -> b -> c) -> f a -> f b -> m (f c)
\end{code}