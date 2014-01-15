% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Id] {The Base Monad}

The base monad has no features at all; it merely passes around values.

\begin{comment}
\begin{code}
module Id where
\end{code}
\end{comment}

\begin{code}
data Id a = Id a 
	  deriving Show

unId :: Id a -> a
unId (Id x) = x

instance Functor Id where
	fmap f (Id v) = Id (f v)

instance Monad Id where
 	return        = Id
 	(Id m) >>= f  = f m

\end{code}