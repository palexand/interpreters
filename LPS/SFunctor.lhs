% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[SFunctor] {Sum Functor}

\begin{comment}
\begin{code}
module SFunctor 
    ( Sum(..)
    , SubFunctor(..)
    , toS
    ) where
import SubType
import HasParser
import Fix
import MFix
import Monad
\end{code}
\end{comment}

Lifting from type to constructor

\begin{code}
newtype Sum f g x = S (Either (f x) (g x))
unS (S x) = x

instance ( Functor f
         , Functor g) 
    => Functor (Sum f g) 
 where fmap g = 
           S . either (Left  . fmap g) 
		      (Right . fmap g) . unS

instance ( HasParserF f
	 , HasParserF g) 
    => HasParserF (Sum f g) 
 where parserF = fmap S (fmap Left  parserF 
				`mplus` 
			 fmap Right parserF)

instance ( AlgebraC f a
	 , AlgebraC g a) 
    => AlgebraC (Sum f g) a 
 where phi = either phi phi . unS

instance ( MFunctor f
	 , MFunctor g) 
    => MFunctor (Sum f g) 
 where mfmap f = either (mLeft  @@ mfmap f) 
			(mRight @@ mfmap f) . unS

mLeft  ::(Monad m) 
	 => MFun m (f x) (Sum f g x)
mLeft  = return . S . Left 

mRight ::(Monad m) 
	 => MFun m (g x) (Sum f g x)
mRight = return . S . Right

instance ( MAlgebraC m f a
	 , MAlgebraC m g a ) 
    => MAlgebraC m (Sum f g) a 
  where 
    mPhi = either mPhi mPhi . unS

\end{code}

It is possible to define a |subtype| relationship between functors. 

\begin{code}

class SubFunctor f g where 
    injF :: f x -> g x
    prjF :: g x -> Maybe (f x)

instance SubFunctor f (Sum f x) where
    injF = S . Left
    prjF (S (Left f)) = Just f
    prjF (S (Right x)) = Nothing

instance ( SubFunctor f g
	 ) => SubFunctor f (Sum x g) where
 injF       = S . Right . injF
 prjF (S (Left x)) = Nothing
 prjF (S (Right b)) = prjF b


toS :: ( SubFunctor a b
       , Functor b
       ) => a (Fix b) -> Fix b
toS x = (inF . injF) x

\end{code}

