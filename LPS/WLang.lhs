An example of a While language. 

This language is very similar to the 
language described in \cite{SlonnegerKurtz95}

\begin{comment}
\begin{code}
module WLang ( Ident(..)
	     , Program(..)
	     , Block(..)
	     , Decl(..)
	     , Args(..)
	     , Type(..)
	     , Comm(..)
	     , ExprArgs(..)
	     , Expr(..)
	     , BOp(..)
	     , UOp(..)
	     ) where
\end{code}
\end{comment}

\begin{code}

type Ident   = String
data Program = Prg Ident Block

data Block = Block Decls Comm

type Decls  = [Decl]

data Decl =  DConst Ident Expr
	   | DVar Ident Type
	   | DProc Ident Args Block

type Args = [Ident]

data Type = TBool
	  | TInt

data Comm = Seq Comm Comm
	  | Assign Ident Expr
	  | Skip
	  | IF Expr Comm Comm
	  | While Expr Comm
	  | Declare Block
	  | Call Ident ExprArgs
	  | Read Ident
	  | Write Expr

type ExprArgs = [Expr]

data Expr = Con Integer
	  | Var Ident
	  | ETrue
	  | EFalse
	  | BinOp Expr BOp Expr
	  | UnOp UOp Expr

data BOp = Add | Sub | Mul | Dvd 
	 | Or | And 
	 | BLT | BLE | BGE | BGT | BEQ | BNEQ

data UOp = Negate | Not

ex :: Program
ex = Prg "test" 
       (Block 
	 [ DConst "c" (Con 13)
	 , DVar "a" TInt 
	 , DVar "b" TBool
	 ]
	 (Seq (Assign "a" (Con 10))
	   (Seq (Assign "b" (BinOp (Var "a") Add (Var "c")))
	     (Seq (Write (Var "b"))
	       Skip
	     ))
	 )
       )

\end{code}

