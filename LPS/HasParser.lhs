% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[HasParser] {HasParser and HasParserF type classes}

\begin{comment}
\begin{code}
module HasParser ( HasParser(parser)
                 , HasParserF(parserF)
                 ) where
import Parsec
import ParsecToken
import Fix
\end{code}
\end{comment}

\begin{code}
class HasParser t where
 parser :: Parser t

class HasParserF f where
 parserF :: (HasParser x) => Parser (f x)

-- HasParserF is needed because Hugs doesn't allow the next declaration:
-- instance HasParser (f (Fix f)) => HasParser (Fix f) where
--   parser = fmap In parser

instance ( Functor f
	 , HasParserF f) 
    => HasParser (Fix f) where
 parser = fmap inF parserF

\end{code}

