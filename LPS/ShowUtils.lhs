% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[ShowUtils] {ShowS Utilities}

\begin{code}

module ShowUtils where

showExpr x op y = showChar '(' . 
                  x . 
	          showChar ' ' . showString op . showChar ' ' . 
                  y . 
                  showChar ')'

nl :: ShowS
nl = showChar '\n'

\end{code}