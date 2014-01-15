An example of a While language. 

This language is very similar to the 
language described in \cite{SlonnegerKurtz95}

\begin{comment}
\begin{code}
module WParser ( wParser
	       ) where
import WLang
import Parsec
import qualified ParsecToken as P
import ParsecExpr
import ParsecLanguage
import ParserUtils
\end{code}
\end{comment}

Parser of While Programs

\begin{code}

wParser = program 

program = 
    do{ reserved "program"
      ; id <- identifier
      ; reserved "is"
      ; blk <- block
      ; return $ Prg id blk
      }

block =
    do{ ds <- decls
      ; reserved "begin"
      ; com <- commSeq
      ; reserved "end"
      ; return $ Block ds com
      }

decls =
    many decl

decl = 
    dConst <|> dVar <|> dProc

dConst =
    do { reserved "const"
       ; id <- identifier
       ; equal
       ; e <- expr
       ; return $ DConst id e
       }

dVar = 
    do { reserved "var"
       ; id <- identifier
       ; colon
       ; t <- typeDecl
       ; return $ DVar id t
       }

typeDecl = 
    typeInt <|> typeBool

typeInt = 
    do{ reserved "int" 
      ; return $ TInt
      }

typeBool = 
    do{ reserved "bool" 
      ; return $ TBool
      }

dProc = 
    do{ reserved "procedure"
      ; id <- identifier
      ; args <- parens (commaSep identifier)
      ; reserved "is"
      ; blk <- block
      ; return $ DProc id args blk
      }

commSeq =
    do{ cs <- semiSep comm
      ; return $ foldr (\c r -> Seq c r) Skip cs
      }

comm =
    while <|> ifComm <|> declare 
    <|> pRead <|> pWrite 
    <|> simpleComm

while =
    do{ reserved "while"
      ; e <- expr
      ; reserved "do"
      ; c <- commSeq
      ; reserved "endWhile"
      ; return $ While e c
      }

ifComm =
    do{ reserved "if"
      ; e <- expr
      ; reserved "then"
      ; c1 <- commSeq
      ; reserved "else"
      ; c2 <- commSeq
      ; return $ IF e c1 c2
      }

declare =
    do{ reserved "declare"
      ; blk <- block
      ; return $ Declare blk
      }

pRead =
    do{ reserved "read"
      ; id <- identifier
      ; return $ Read id
      }

pWrite =
    do{ reserved "write"
      ; e <- expr
      ; return $ Write e
      }

simpleComm =
    do{ id <- identifier
      ; assign id <|> call id
      }

assign id =
    do{ reservedOp ":="
      ; e <- expr
      ; return $ Assign id e
      }

call id =
    do{ args <- parens (commaSep expr)
      ; return $ Call id args
      }

expr =
    buildExpressionParser ops simpleExpr

-- Todo: unary operator "Not"
ops   = 
    [ [ uop "-" Negate]
    , [ op "*" Mul AssocLeft, op "/" Dvd AssocLeft]
    , [ op "+" Add AssocLeft, op "-" Sub AssocLeft]
    , [ op "==" BEQ AssocNone, op "!=" BNEQ AssocNone, 
	op "<" BLT AssocNone, op "<=" BLE AssocNone, 
	op ">" BGT AssocNone, op ">=" BGE AssocNone ]
    , [ op "&&" And AssocNone ]
    , [ op "||" Or AssocNone ]
    ]
    where
      op name op assoc
        = Infix (do{ try (symbol name)
		   ; return (\e1 e2 -> BinOp e1 op e2)
		   }) assoc
      uop name op 
        = Prefix (do{ try (symbol name)
		    ; return (\e -> UnOp op e)
		    })

simpleExpr :: Parser Expr    
simpleExpr =
        literal
    <|> parens expr
    <|> variable            
    <?> "simple expression"

literal = 
       natural <$> Con
   <|> bool
   
bool = reserved "true"  <$> const ETrue
  <|>  reserved "false" <$> const EFalse

variable = 
    var <$> Var

var = identifier <?> "identifier"

equal = reservedOp "="

\end{code}

Tokens

\begin{code}

lang    = P.makeTokenParser 
  (javaStyle{ 
    reservedNames 
      = [ "program", "is"
	, "begin", "end"
	, "const", "var"
	, "int", "bool"
	, "procedure"
	, "while", "do", "endwhile"
	, "if", "then", "else"
	, "declare"
	, "read", "write"
	, "true", "false"
	]
  , reservedOpNames
     = [ "=" ] 
  })
    
parens          = P.parens lang    
braces          = P.braces lang    
semiSep         = P.semiSep lang    
whiteSpace      = P.whiteSpace lang    
symbol          = P.symbol lang    
identifier      = P.identifier lang    
reserved        = P.reserved lang    
reservedOp      = P.reservedOp lang    
integer         = P.integer lang
natural         = P.natural lang
colon           = P.colon lang
semi            = P.semi lang
commaSep        = P.commaSep lang

\end{code}
