% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[IOUtils] {Input Output Monad}

\begin{comment}
\begin{code}
module IOMonad 
          where

\end{code}
\end{comment}

\begin{code}

class (Monad m) => IOMonad m where
 get :: m Char
 put :: Char -> m ()

 mPutStrLn :: String -> m ()
 mPutStrLn s = 
     do { sequence_ $ map put s
	; put '\n'
	}

 mGetLine  :: m String
 mGetLine = do{ c <- get
	      ; if c == '\n' then return []
		else do{ cs <- mGetLine
		       ; return (c:cs)
		       }
	      }

instance IOMonad IO where
 put = putChar
 get = getChar


\end{code}
