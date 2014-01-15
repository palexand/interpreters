\section{Abstract Syntax}

\begin{code}
  {-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
  module SystemFAST
      where
      
  import LangUtils
\end{code}

\subsection{Type Language}

The \emph{type language} defines the abstract syntax used to describe
type declarations.  Because general terms cannot appear in type
declarations, the type language is specified separately and explicitly
interpreted when a type is to appear in the langauge abstract syntax.

\begin{code}
  -- Define type language elements
  data TyBase ty = TyBool | TyInt | TyUnit deriving (Eq,Show)

  data TyAbs ty = ty :->: ty deriving (Eq,Show)

  data TyTuple ty = TyTuple [ty] deriving (Eq,Show)

  data TyVar ty = TyVar String deriving (Eq,Show)

  data TySum ty = ty :+: ty deriving (Eq,Show)

  -- Define type language as sum
  type TyLangSum = (Sum TyBase (Sum TyAbs (Sum TySum (Sum TyTuple TyVar))))

  type TyLang = Rec TyLangSum
\end{code}

\subsection{Term Language}

The \emph{term language} defines the abstract syntax of general
language terms up to types.

\subsubsection{Base Terms}

\begin{code}
  data TmBool te = TmTrue | TmFalse deriving (Eq,Show)

  instance Functor TmBool where
      fmap f TmTrue = TmTrue
      fmap f TmFalse = TmFalse

  data TmInt te = TmConstInt Int deriving (Eq,Show)

  instance Functor TmInt where
      fmap f (TmConstInt x) = (TmConstInt x)

  data TmUnit te = TmUnit deriving (Eq,Show)
              
  instance Functor TmUnit where
      fmap f TmUnit = TmUnit
\end{code}

\subsubsection{Tuples}

\begin{code}
  data TmTuple te
      = TmTuple [te]
      | TmPrj Int te
        deriving (Eq,Show)
                  
  instance Functor TmTuple where
      fmap f (TmTuple x) = TmTuple (map f x)
      fmap f (TmPrj x y) = TmPrj x (f y)
\end{code}

\subsubsection{Sums}

\begin{code}
  data TmSum te
      = InL te TyLang | InR te TyLang | TmCase te TyLang String te String te

  instance Functor TmSum where
      fmap f (InL x t) = InL (f x) t
      fmap f (InR x t) = InR (f x) t
      fmap f (TmCase v ty lt lv rt rv) = TmCase (f v) ty lt (f lv) rt (f rv)

\end{code}

\subsubsection{Operations}

\begin{code}

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
\end{code}

\subsubsection{IF expression}

\begin{code}
  data TmIf te = If te te te deriving (Eq,Show)

  instance Functor TmIf where
      fmap f (If c t e) = (If (f c) (f t) (f e))
\end{code}

\subsubsection{Functions, Variables and Applications}

\begin{code}
  data TmVar t = TmVar String 
               | TmTVar String
                 deriving (Show,Eq)

  instance Functor TmVar where
      fmap f (TmVar x) = (TmVar x)
      fmap f (TmTVar x) = (TmTVar x)

  data TmFn t = TmLambda String TyLang t
              | TmApp t t
              | TmTLambda String t
              | TmTApp t TyLang

  instance Functor TmFn where
      fmap f (TmLambda s ty te) = (TmLambda s ty (f te))
      fmap f (TmApp te1 te2) = (TmApp (f te1) (f te2))
      fmap f (TmTLambda s te) = (TmTLambda s (f te))
      fmap f (TmTApp te1 ty1) = (TmTApp (f te1) ty1)
\end{code}

\subsubsection{Term Language Sum}

\begin{code}
  type TmLangSum = (Sum TmBool
                    (Sum TmInt
                     (Sum TmUnit
                      (Sum TmOp
                       (Sum TmTuple
                        (Sum TmSum
                         (Sum TmIf
                          (Sum TmVar TmFn))))))))

  type TmLang = Rec TmLangSum


  toTmLang :: (Subsum f TmLangSum) => f TmLang -> TmLang
  toTmLang = toSum

\end{code}
