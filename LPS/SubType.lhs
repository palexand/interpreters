% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[SubType] {Extensible Union Types}

\begin{comment}
\begin{code}
module SubType (SubType, inj, prj, returnInj) where
\end{code}
\end{comment}

\begin{code}
class SubType a b where
 inj :: a -> b       
 prj :: b -> Maybe a 

instance SubType a (Either a x) where
 inj       = Left
 prj       = either Just (const Nothing)

instance ( SubType a b
	 ) => SubType a (Either x b) where
   inj       = Right . inj
   prj       = either (const Nothing) prj

returnInj::( SubType a b
	   , Monad m
	   ) => a -> m b
returnInj = return . inj

\end{code}