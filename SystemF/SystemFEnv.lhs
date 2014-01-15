\section{Environment}

This very simple module defines a standard environment parameterized
over a stored type.  It is used to define both |Gamma| for the type
checking routine and the environment for the evaluation routine.

\begin{code}
  module SystemFEnv where

  type Environment a = [(String,a)]

  lookupEnv :: (Eq a) => String -> (Environment a) -> (Maybe a)
  lookupEnv s e = lookup s e
\end{code}