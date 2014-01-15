% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Initial] {Initial type class}

\begin{code}

module Initial where

class Initial x where
  initial :: x  

-- Some instances 

instance Initial () where initial = ()
instance (Num n) => 	Initial 	n 	where initial = 0  
instance Initial y => 	Initial (Either x y)    where initial = Right initial


\end{code}
