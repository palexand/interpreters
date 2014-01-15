% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[ParserUtils] {Common Parser Utilities}

\begin{comment}
\begin{code}
module ParserUtils where
import Parsec

infixl <$>
\end{code}
\end{comment}

\begin{code}

(<$>) :: Monad a => a b -> (b -> c) -> a c
parser <$> f = do{ x <- parser; return (f x); }

check :: Parser a -> Parser ()
check p = do { p; return () }

pFileName :: Parser String
pFileName = many (alphaNum <|> satisfy isSep)

isSep :: Char -> Bool
isSep x = elem x ['/', '\\', '.', '~']

\end{code}