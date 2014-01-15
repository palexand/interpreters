% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[LParser] {Language Parser}

\begin{comment}
\begin{code}
module LParser( pExpr
	      ) where
import LLang
import Id
import Fix
import Parsec
import qualified ParsecToken as P
import ParsecExpr
import ParsecLanguage
import SFunctor
import SubType
import ParserUtils
\end{code}
\end{comment}

Abstract Syntax Tree

\begin{code}

data Expr  = ADD Expr Expr | SUB Expr Expr 
	   | MUL Expr Expr | DIV Expr Expr
	   | INT  Integer
	   | BOOL Bool | OR Expr Expr | AND Expr Expr
	   | IF Expr Expr Expr
	   | LE Expr Expr | GR Expr Expr 
	   | LEQ Expr Expr | GEQ Expr Expr
	   | EQUAL Expr Expr | NEQUAL Expr Expr
	   | VAR VName
	   | NEW Expr | GET Expr 
	   | ASSIGN Expr Expr | SEQ Expr Expr
	   | LETV VName Expr Expr | LETN VName Expr Expr 
	   | LETL VName Expr Expr
	   | LAMBDAN VName Expr | LAMBDAV VName Expr 
	   | LAMBDAL VName Expr
	   | APP Expr Expr
	   | TRACE VName Expr
	   | CALLCC
	   | READ Expr | WRITE Expr
    deriving Show

type VName = String
\end{code}

Language Parser

\begin{code}


pLang = wrap expr

wrap p = 
    do{ whiteSpace
      ; x <- p 
      ; eof
      ; return x
      }

expr :: Parser Expr      
expr =
        lambdas
    <|> lets 
--    <|> news
    <|> infixExpr     
    <?> "expression"

infixExpr    = 
    buildExpressionParser ops appExpr

ops   = 
    [ [op "*" MUL AssocLeft, op "/" DIV AssocLeft]
    , [op "+" ADD AssocLeft, op "-" SUB AssocLeft]
    , [ op "==" EQUAL AssocNone, op "/=" NEQUAL AssocNone, 
	op "<" LE AssocNone, op "<=" LEQ AssocNone, 
	op ">" GR AssocNone, op ">=" GEQ AssocNone ]
    , [ op "&&" AND AssocNone ]
    , [ op "||" OR AssocNone ]
    , [ op ":=" ASSIGN AssocNone]
    , [ op ";"  SEQ AssocLeft]
    ]
    where
      op name f assoc
             = Infix (do{ try (symbol name)
			; return f
			}) assoc

appExpr =
    do{ exprs <- many1 simpleExpr
      ; return (foldl1 APP exprs)
      }

simpleExpr :: Parser Expr    
simpleExpr =
        literal
    <|> parens expr
--    <|> caseExpression
    <|> ifExpr
    <|> pNew  <|> pGet 
    <|> pRead <|> pWrite
    <|> variable            
    <?> "simple expression"

literal = 
       natural <$> INT
   <|> bool <$> BOOL
   <|> callcc
   
bool = reserved "true"  <$> const True
  <|>  reserved "false" <$> const False

callcc = reserved "callcc" >> return CALLCC

ifExpr = 
    do{ reserved "if"
      ; cond <- expr
      ; reserved "then"
      ; e1 <- expr
      ; reserved "else"
      ; e2 <- expr
      ; return (IF cond e1 e2)
      }

simple str con =
    do{ reserved str
      ; e <- expr
      ; return $ con e
      }

pNew   = simple "new" NEW
pGet   = simple "get" GET
pRead  = simple "read" READ
pWrite = simple "write" WRITE

variable = var <$> VAR

var = identifier <?> "identifier"

lets = plet "letL" LETL 
   <|> plet "letV" LETV 
   <|> plet "letN" LETN

lambdas = lambda "\\L" LAMBDAL 
      <|> lambda "\\V" LAMBDAV
      <|> lambda "\\N" LAMBDAN

lambda str con = do{ reserved str
		   ; x <- var
		   ; reservedOp "->"
		   ; e <- expr
		   ; return $ con x e
		   }

plet str con = do{ reserved str
		 ; x <- var
		 ; reservedOp "="
		 ; e1 <- expr
		 ; reserved "in"
		 ; e2 <- expr
		 ; return $ con x e1 e2
                 }

\end{code}

Conversion from abstract syntax tree to semantic expressions

language 

\begin{code}

pExpr :: Parser LSyntax
pExpr = pLang <$> cnv

cnv :: Expr -> LSyntax
cnv (ADD e1 e2) = toS $ Add (cnv e1) (cnv e2)
cnv (SUB e1 e2) = toS $ Sub (cnv e1) (cnv e2)
cnv (MUL e1 e2) = toS $ Mul (cnv e1) (cnv e2)
cnv (DIV e1 e2) = toS $ Dvd (cnv e1) (cnv e2)
cnv (INT n)     = toS $ Num n
cnv (BOOL b)    = toS $ BCons b
cnv (EQUAL e1 e2) = toS $ CEQ (cnv e1) (cnv e2)
cnv (NEQUAL e1 e2) = toS $ CNEQ (cnv e1) (cnv e2)
cnv (GR e1 e2) = toS $ CGT (cnv e1) (cnv e2)
cnv (LE e1 e2) = toS $ CLT (cnv e1) (cnv e2)
cnv (LEQ e1 e2) = toS $ CLE (cnv e1) (cnv e2)
cnv (GEQ e1 e2) = toS $ CGE (cnv e1) (cnv e2)
cnv (AND e1 e2) = toS $ And (cnv e1) (cnv e2)
cnv (OR e1 e2) = toS $ Or (cnv e1) (cnv e2)
cnv (IF c e1 e2) = toS $ Cond (cnv c) (cnv e1) (cnv e2)
cnv (VAR x)    = toS $ Var x
cnv (LETV x e1 e2) = toS $ LetV x (cnv e1) (cnv e2)
cnv (LETN x e1 e2) = toS $ LetN x (cnv e1) (cnv e2)
cnv (LETL x e1 e2) = toS $ LetL x (cnv e1) (cnv e2)
cnv (LAMBDAV x e)   = toS $ LambdaV x (cnv e)
cnv (LAMBDAN x e)   = toS $ LambdaN x (cnv e)
cnv (LAMBDAL x e)   = toS $ LambdaL x (cnv e)
cnv (APP e1 e2)     = toS $ App (cnv e1) (cnv e2)
cnv (ASSIGN e1 e2)  = toS $ Assign (cnv e1) (cnv e2)
cnv (SEQ e1 e2)     = toS $ Seq (cnv e1) (cnv e2)
cnv (NEW e)         = toS $ New (cnv e)
cnv (GET e)         = toS $ Get (cnv e)
cnv (READ e)        = toS $ Read (cnv e)
cnv (WRITE e)       = toS $ Write (cnv e)
cnv CALLCC          = toS $ Callcc
cnv _          = error "cnv: Unknown case"

\end{code}

Tokens

\begin{code}

lang    = P.makeTokenParser 
            (haskellStyle{ 
	       reservedNames = [ "true"
			       , "false"
			       , "if", "then", "else"
			       , "letV", "letN", "letL", "in"
			       , "\\L", "\\V", "\\N"
			       , "get", "new"
			       , "read", "write"
			       ]
	     , reservedOpNames=["=","->"] 
	                 {-"/=","==",
			    "+","-","*","/",
			    "<=",">=",">","<",
			    "->" ]-}
			 })
    
parens          = P.parens lang    
braces          = P.braces lang    
semiSep1        = P.semiSep1 lang    
whiteSpace      = P.whiteSpace lang    
symbol          = P.symbol lang    
identifier      = P.identifier lang    
reserved        = P.reserved lang    
reservedOp      = P.reservedOp lang    
integer         = P.integer lang
natural         = P.natural lang

\end{code}
