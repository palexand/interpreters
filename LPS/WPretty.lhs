An example of a While language. 

This language is very similar to the 
language described in \cite{SlonnegerKurtz95}

\begin{comment}
\begin{code}
module WPretty (
	       ) where
import WLang
import Pretty
\end{code}
\end{comment}

Pretty Printing

\begin{code}
instance Show Program where
  showsPrec d program   = shows (ppProgram program)

ppProgram (Prg id block)
    = text ("Program") <+> text id <+> text "is" $$ 
      ppBlock block

ppBlock (Block decls comm)
    = vcat (map ppDecl decls) $$ 
      text "begin" $$
      ppComm comm  $$
      text "end"
	

ppDecl (DConst id e) 
    = text "const" <+> text id <+> eq <+> ppExpr e
      
ppDecl (DVar id t)
    = text "var" <+> text id <+> eq <+> ppType t

ppDecl (DProc id args bl) 
    = text "procedure" <+> text id <+> 
      ppArgs args <+> text "is" $$
      ppBlock bl

ppArgs args =
    parens $ hsep $ punctuate comma $ map text args


ppType TBool = text "Bool"
ppType TInt  = text "Int"

ppComm (Seq c1 c2) 
  = ppComm c1 <+> semi $$ ppComm c2

ppComm (Assign id e) 
  = text id <+> eq <+> ppExpr e
ppComm Skip 
  = text "skip"

ppComm (IF e c c') 
  = text "if" <+> ppExpr e $$ 
    text "then" <+> ppComm c $$
    text "else" <+> ppComm c'

ppComm (While e c) 
  = text "while" <+> ppExpr e <+> text "do" $$ 
    ppComm c

ppComm (Declare block) 
  = brackets $ ppBlock block

ppComm (Call id eas)
  = text "call" <+> text id <+> 
    (parens $ hsep $ punctuate comma $ map ppExpr eas)

ppComm (Read id)
  = text "read" <+> text id

ppComm (Write e)
  = text "write" <+> ppExpr e

ppExpr (Con n) = text $ show n
ppExpr (Var v) = text v
ppExpr ETrue   = text "true"
ppExpr EFalse  = text "false"
ppExpr (BinOp e op e') 
    = parens $ ppExpr e <+> ppOp op <+> ppExpr e'
ppExpr (UnOp uop e)
    = parens $ ppUOp uop <+> ppExpr e

ppOp Add = text "+"
ppOp Sub = text "-"
ppOp Mul = text "*"
ppOp Dvd = text "/"
ppOp Or  = text "||"
ppOp And = text "&&"
ppOp BLT = text "<"
ppOp BLE = text "<="
ppOp BGT = text ">"
ppOp BGE = text ">="
ppOp BEQ = text "=="
ppOp BNEQ = text "!="

ppUOp Negate = text "-"
ppUOp Not    = text "not"

undef = text "pp undefined"
    
eq = text "="

\end{code}

