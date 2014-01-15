\section{Abstract Syntax}

\begin{code}
  module TypedLambdaExtendedAST
      where
      
  import LangUtils
\end{code}

\subsection{Type Language}

\begin{code}
  data TyBase ty = TyBool | TyInt deriving (Eq,Show)

  data TyAbs ty = ty :->: ty deriving (Eq,Show)

  data TyTuple ty = TyTuple [ty] deriving (Eq,Show)

  type TyLangSum = (Sum TyBase (Sum TyAbs TyTuple))

  type TyLang = Rec TyLangSum

--  instance Eq TyLang where
--      x == y = (unS (out x)) == (unS (out y))
\end{code}

\subsection{Term Language}

The term language include Boolean values and integer values, addition
and subtraction operators, if-then-else expressions, and lambda
expressions and lambda application.

\begin{code}
  data TmBool te = TmTrue | TmFalse deriving (Eq,Show)

  instance Functor TmBool where
      fmap f TmTrue = TmTrue
      fmap f TmFalse = TmFalse

  data TmInt te = TmConstInt Int deriving (Eq,Show)

  instance Functor TmInt where
      fmap f (TmConstInt x) = (TmConstInt x)

  data TmTuple te
      = TmTuple [te]
      | TmPrj Int te
	deriving (Eq,Show)
		  
  instance Functor TmTuple where
      fmap f (TmTuple x) = TmTuple (map f x)
      fmap f (TmPrj x y) = TmPrj x (f y)

  data TmOp te
      = TmAdd te te
      | TmSub te te 
      | TmMul te te
      | TmDiv te te
	deriving (Eq,Show)

  instance Functor TmOp where
      fmap f (TmAdd x y) = (TmAdd (f x) (f y))
      fmap f (TmSub x y) = (TmSub (f x) (f y))
      fmap f (TmMul x y) = (TmMul (f x) (f y))
      fmap f (TmDiv x y) = (TmDiv (f x) (f y))

  data TmIf te = If te te te deriving (Eq,Show)

  instance Functor TmIf where
      fmap f (If c t e) = (If (f c) (f t) (f e))

  data TmVar t = TmVar String deriving (Show,Eq)

  instance Functor TmVar where
      fmap f (TmVar x) = (TmVar x)

  data TmFn t = TmLambda String TyLang t
	      | TmApp t t
--		deriving (Eq)

  instance Functor TmFn where
      fmap f (TmLambda s ty te) = (TmLambda s ty (f te))
      fmap f (TmApp te1 te2) = (TmApp (f te1) (f te2))

  type TmLangSum = (Sum TmBool
		    (Sum TmInt
		     (Sum TmOp
		      (Sum TmTuple
		       (Sum TmIf
			(Sum TmVar TmFn))))))

  type TmLang = Rec TmLangSum


  toTmLang :: (Subsum f TmLangSum) => f TmLang -> TmLang
  toTmLang = toSum

\end{code}
