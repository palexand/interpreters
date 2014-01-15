% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Main] {Main program}

\begin{comment}
\begin{code}
module Main where
import Lang
import Shell
import IOUtils
\end{code}
\end{comment}

\begin{code}
main :: IO ()
main = do { putStrLn title
	  ; shell askAction checkQuit answer s0
          ; putStrLn bye
          }

\end{code}