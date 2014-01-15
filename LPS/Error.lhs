% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Error]{Error control}

The |Error| datatype is similar to |Maybe| but carries an
error message

\begin{comment}
\begin{code}
module Error ( Error
             , ifError
             , ok
             , raise) where
\end{code}
\end{comment}

\begin{code}
newtype Error a = E (Either String a)

ok    = E . Right
raise = E . Left

ifError::(String -> a) -> (b -> a) -> Error b -> a
ifError f g (E x) = either f g x

instance Functor Error where
	fmap f e = ifError raise (ok . f) e

instance Monad Error where
	m >>= f = ifError raise f m
	return  = ok

instance Show v => Show (Error v) where
	show = ifError id show

\end{code}
