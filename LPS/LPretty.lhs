% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[LPretty] {Functional Language Pretty Printer}

\begin{comment}
\begin{code}
module LPretty
    ( 
    ) where
import LLang
import Fix
import ShowUtils
\end{code}
\end{comment}

\begin{code}

instance Show LSyntax 
    where showsPrec p = pCata 

instance AlgebraC U ShowS where
  phi Undef    = showString "Undefined"

instance AlgebraC N ShowS 
 where	
   phi (Num n)     = shows n
   phi (x `Add` y) = showExpr x "+" y
   phi (x `Sub` y) = showExpr x "-" y
   phi (x `Mul` y) = showExpr x "*" y
   phi (x `Dvd` y) = showExpr x "/" y

instance Show (Fix N) where
 showsPrec p = pCata 

instance AlgebraC V ShowS 
 where phi (Var id) = 
	   showString "var " . showString id

instance Show (Fix V) where
 showsPrec p = pCata

instance AlgebraC B ShowS 
 where
  phi (BCons b)    = shows b
  phi (x `And` y)  = showExpr x "&&" y
  phi (x `Or`  y)  = showExpr x "||" y
  phi (Cond x y z) = showString " if "   . x . 
		     showString " then " . y .
		     showString " else " . z

instance AlgebraC CMP ShowS where
	phi (x `CLT`  y) = showExpr x "<" y
	phi (x `CLE`  y) = showExpr x "<=" y
	phi (x `CGT`  y) = showExpr x ">" y
	phi (x `CGE`  y) = showExpr x ">=" y
	phi (x `CEQ`  y) = showExpr x "==" y
	phi (x `CNEQ` y) = showExpr x "!=" y

instance AlgebraC D ShowS 
 where
  phi (LetV n x y) = 
      showString " LetV " . 
      showString n . 
      showString " = " . x .
      showString " in " . y
  
  phi (LetN n x y) = 
      showString " LetN " . 
      showString n . 
      showString " = " . x .
      showString " in " . y
  
  phi (LetL n x y) = 
      showString " LetL " . 
      showString n . 
      showString " = " . x .
      showString " in " . y

instance Show (Fix D) where
 showsPrec p = pCata

instance AlgebraC F ShowS 
 where 	
   phi (LambdaN n x) = 
       showString "\\N " . 
       showString n . 
       showString " -> " . 
       x
   
   phi (LambdaV n x) = 
       showString "\\V " . 
       showString n . 
       showString " -> " . x
	
   phi (LambdaL n x) = 
       showString "\\L " . 
       showString n . 
       showString " -> " . x
	
   phi (App e1 e2) = 
       showChar '(' . e1 . 
       showChar ' ' . e2 . 
       showChar ')'

instance Show (Fix F) where
 showsPrec p = pCata

instance AlgebraC R ShowS 
 where	phi (New x)      = showString "new " . x
	phi (Get x)      = showString "get " . x
	phi (Assign x y) = showExpr x ":=" y
	phi (Seq x y)    = showExpr x "; " y

instance Show (Fix R) where
 showsPrec p = pCata 

instance AlgebraC TIO ShowS 
 where	
   phi (Read x)  = showString "read " . x
   phi (Write x) = showString "write " . x

instance Show (Fix TIO) where
 showsPrec p = pCata 

instance AlgebraC Callcc ShowS 
 where phi Callcc = showString "callcc "

instance Show (Fix Callcc) where
 showsPrec p = pCata

instance AlgebraC P ShowS 
 where 
  phi (Print n) = 
      showString "print " . 
      showString n
      
  phi (ShowMsg msg) = 
      showString "msg " . 
      shows msg


\end{code}


