% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Basics] {Basic declarations}

\begin{comment}
\begin{code}
module Basics ( Msg
              , Name 
              , Loc 
              , Level
	      , Bottom
              ) where
import Initial
\end{code}
\end{comment}

Some common types:
\begin{code}
type Msg 	= String
type Name 	= String
type Loc 	= Integer
type Level 	= Integer
\end{code}

\begin{code}
data Bottom = Undefined deriving Show

instance Initial Bottom 
    where initial = Undefined
\end{code}