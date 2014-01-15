% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[TokenDef] {Token Definition}

\begin{comment}
\begin{code}
module TokenDef( tokenDef ) where
import StdTokenDef
\end{code}
\end{comment}

This module defines the tokens needed by the |Parsec| library

\begin{code}
tokenDef  = haskellStyle { 
    reservedNames = 
	 [ "PUSHC", "PUSHA", "LOAD", "STORE"
	 , "ADD", "SUB", "MUL", "DIV", "INT"
	 , "JMPLZ", "JMPZ", "GOTO", "READ", "WRITE" 
	 ]}

\end{code}