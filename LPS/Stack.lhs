%
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[IL] {Instructions on the Intermediate Language}

\begin{comment}
\begin{code}
module Stack ( Stack
	     , emptyStack
	     , push
	     , pop
	     , pop2
	     ) where
\end{code}
\end{comment}

\begin{code}
data Stack v = S [v] deriving Show

emptyStack = S []

push v (S s) = S (v:s)

pop  (S (v:s)) = Just (v,S s)
pop  (S [])    = Nothing

pop2 s = case pop s of
	  Nothing      -> Nothing
	  Just (v1,s1) -> case pop s1 of
			   Nothing      -> Nothing
			   Just (v2,s2) -> Just (v1,v2,s2)

\end{code}