% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[HasName]{Types with name}

\begin{comment}
\begin{code}
module HasName where
import SubType
\end{code}
\end{comment}

Get the name of a subtype in a union. Potentially useful for run-time
type information.

\begin{code}
class HasName a where
    name :: a -> String

instance Show a => HasName a where
        name x = show x 

\end{code}
\begin{comment}
\begin{code}
{-

instance HasName () where
        name x = "()"

instance (HasName a, HasName b, Sum sum) => HasName (sum a b) where
    name = name <+> name

instance HasName Int where
    name x = "Int"

instance HasName Bool where
    name x = "Bool"

-}
\end{code}
\end{comment}