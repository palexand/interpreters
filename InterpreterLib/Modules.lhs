\begin{code}
module InterpreterLib.Modules where

class Module mod opened | mod -> opened where
  open :: mod -> opened
\end{code}