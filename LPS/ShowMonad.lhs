% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[ShowMonad] {A Monad with a |showMonad| operation}

\begin{comment}
\begin{code}
module ShowMonad(ShowMonad, showMonad) where
import MonadT
\end{code}
\end{comment}

\begin{code}

class (Monad m) => ShowMonad m where
 showMonad::m ShowS -> ShowS

instance ( ShowMonad m
	 , Functor m
	 , Show a) => Show (m a) 
    where
	showsPrec p = showMonad . fmap shows


\end{code}