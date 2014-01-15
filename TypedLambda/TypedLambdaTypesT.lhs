\section{Type Checking}

\subsection{Type Values}

These are the type values available in our language.  For the type
language, this will serve as the carrier set or value space for both
the type langauge and the term language under type checking.  |phi|
for the type language is defined over |Ty a| while |phi| for the term
language type checker is defined over |Tm a|.  In effect, |phi|
evaluates the term language to a type value rather than a term value.

\begin{code}
module TypedLambdaTypesT where
  import LangUtils
  import TypedLambdaAST
  import TypedLambdaEnv
  import Monad
  import Control.Monad.Error
  import Control.Monad.Reader
\end{code}

Note that values are not interpreted, so no |Algebra| is needed.
Technically, we could make |phi = id| for values, but it's not
necessary to think about this right now.

\subsubsection{Boolean and Integer Type Value}

\begin{code}
  data TyBaseVal ty = TyBoolVal | TyIntVal deriving (Eq,Show)
  
  instance Functor TyBaseVal where
      fmap f TyBoolVal = TyBoolVal
      fmap f TyIntVal = TyIntVal
\end{code}

\subsubsection{Abstraction Type Value}

\begin{code}
  data TyAbsVal ty = TyAbsVal ty ty deriving (Eq,Show)

  instance Functor TyAbsVal where
      fmap f (TyAbsVal x y) = TyAbsVal (f x) (f y)
\end{code}

\subsubsection{Type Value}

The value space sum for types is the sum of the base values (integer
and boolean) and the abstraction value and is called |TyValSum|.  The
set of type values is the fixed point, |TyVal|.  |TyVal| is an
instance of |Show| and |Eq| to allow printing and comparing values.
|toTyVal| injects elements from |TyVal| components into the value
space.

\begin{code}
  type TyValSum = (Sum TyBaseVal TyAbsVal)
 
  instance (Show (f a), Show (g a)) => Show (Sum f g a) where
      show (S (Prelude.Left x)) = ("(Left " ++ (show x) ++ ")")
      show (S (Prelude.Right x)) = ("(Right " ++ (show x) ++ ")")

  type TyVal = Rec TyValSum

  instance Show TyVal where
      show x = show (out x)

  instance Eq TyVal where
      x == y = (unS (out x)) == (unS (out y))

  toTyVal :: (Subsum f TyValSum) => f TyVal -> TyVal
  toTyVal = toSum
\end{code}

\subsection{The Reader Error Monad}

The monad used for handling the environment and error messages will be
formed by composing a |Reader| with and |ErrorMonad|.  First we define
the error handling aspects, then embed the |ErrorMonad| in a |Reader|
using |ReaderT|.

The |Either| type constructor is already an instance of the
|MonadError| class.  Thus, it is not necessary to define |throwError|
and |catchError| explicitly for the type.  The definitions are
included here for documentation, but are not loaded.

\begin{spec}
  instance MonadError (Either e) where
      throwError = Left
      catchError (Left e) handler = handler e
      catchError a _ = a
\end{spec}

|TyError| is a simple data type for storing errors. We could simply
store the error string rather than create a type.  However, |TyError|
serves as a placeholder if we want to do fancier things later.
|TyError| is also an instance of the standard |Error|.
 
\begin{code}
  data TyError = Err String deriving (Show,Eq)

  instance Error TyError where
      noMsg = Err "Type Error"
      strMsg s = Err s
\end{code}

|Gamma| defines the data structure used for a binding list.  It is
simply a list of |(String,TyVal)| pairs.  Adding a binding appends it
to the front of a binding list and looking up a binding is handled in
the canonical fashion.

\begin{code}
  type Gamma = Environment TyVal

  addBinding :: Gamma -> (String,TyVal) -> Gamma
  addBinding g t = (t:g)

  lookupGamma :: String -> Gamma -> Maybe TyVal
  lookupGamma = lookup
\end{code}

|TyMonad| defines the actual monad used by the type checker.  The
signature of |TyMonad| is a bit odd.  It must be a type constructor
and thus must have one argument.  |ReaderT| is applied to a |Gamma|
and |(Either TyError)| leaving the last argument to |TyError| as an
argument to |TyMonad|.

\begin{code}
  type TyMonad = ReaderT Gamma (Either TyError) TyVal

  instance Subtype TyError (Either TyError TyVal) where
      inj x = (Left x)
      prj (Left x) = Just x
      prj (Right x) = Nothing

  instance Subtype TyVal (Either TyError TyVal) where
      inj x = (Right x)
      prj (Right x) = Just x
      prj (Left x) = Nothing
\end{code}

\subsection{Type Language}

The type language defines the language for types over the type values.
The type language will be |f| and defined over the type value space
serving as |a| in an algebra definition.

\subsubsection{Base Types}

The Base Types represent integer and boolean atomic types.

\begin{code}
  instance Functor TyBase where
      fmap f TyBool = TyBool
      fmap f TyInt = TyInt

  instance Algebra TyBase TyMonad where
      phi TyBool = return $ inj $ toTyVal TyBoolVal
      phi TyInt = return $ inj $ toTyVal TyIntVal
\end{code}

\subsubsection{Abstraction Type}

Typically thought of as a function type, the abstraction type
represents a mapping from a range type to a domain type.

\begin{code}
  instance Functor TyAbs where
      fmap f (x :->: y) = (f x) :->: (f y)

  instance Algebra TyAbs TyMonad where
      phi (x :->: y) = do { x' <- x
                          ; y' <- y
                          ; return $ inj $ toTyVal (TyAbsVal x' y')
                          }
\end{code}

Define a utility function for converting a type term into the type
language.  The |evalTy| function is a separate function for evaluating
elements of the type language.

\begin{code}
  toTyLang :: (Subsum f TyLangSum) => f TyLang -> TyLang
  toTyLang = toSum

  evalTy :: TyLang -> TyMonad
  evalTy = cata
\end{code}

\subsection{Type Checking Functions}

The type checking functions are defined by defining an algebra from
|TmLang| to |TyMonad|.  Thus, |TyMonad| is the carrier set for the
|TmLang| algebra and |phi| defines the evaluation function.

\begin{code}
  instance Algebra TmBool TyMonad where
      phi TmTrue = return $ inj $ toTyVal TyBoolVal
      phi TmFalse = return $ inj $ toTyVal TyBoolVal

  instance Algebra TmInt TyMonad where
      phi (TmConstInt x) = return $ inj $ toTyVal TyIntVal

  instance Algebra TmOp TyMonad where
      phi (TmAdd x y) = do { x' <- x
                           ; y' <- y
                           ; if (x' == (toTyVal TyIntVal) &&
                                 y' == (toTyVal TyIntVal))
                             then return $ inj $ toTyVal TyIntVal
                             else throwError $ Err "Argument to Add not Integer"
                           }
      phi (TmSub x y) = do { x' <- x
                           ; y' <- y
                           ; if (x' == (toTyVal TyIntVal) &&
                                 y' == (toTyVal TyIntVal))
                             then return $ inj $ toTyVal TyIntVal
                             else throwError $ Err "Argument to Sub not Integer"
                           }

  instance Algebra TmIf TyMonad where
      phi (If c t e) = do { c' <- c
                          ; t' <- t
                          ; e' <- e
                          ; if (c' == (toTyVal TyBoolVal) &&
                                t' == e')
                            then return $ inj t'
                            else throwError $ Err "Either condition is not boolean or then and else are not of same type in If"
                          }

  instance Algebra TmVar TyMonad where
      phi (TmVar s) = do { val <- asks (lookupGamma s)
                         ; case val of 
                           Just x -> return x
                           Nothing -> throwError $ Err ("Variable " ++ (s ++ " not found"))
                         }

  instance Algebra TmFn TyMonad where
      phi (TmLambda s ty te) = do { gamma <- ask
                                  ; ty' <- evalTy ty
                                  ; te' <- local (const (addBinding gamma (s,ty'))) te
                                  ; return $ inj $ toTyVal (TyAbsVal ty' te')
                                  }

      phi (TmApp te1 te2) = do { te1' <- te1
                               ; te2' <- te2
                               ; checkLambda (out te1') te2'
                               }

  checkLambda l te2 = case (prjS l) of
                      (Just (TyAbsVal tty tte)) -> if tty == te2
                                            then return $ inj tte 
                                            else throwError $ Err "Actual parameter type does not match formal parameter type"
                      _ -> throwError $ Err "First argument to application must be a Lambda"
\end{code}

The basic |typeof| function is a catamorphism over the |TmLang
TyMonad|.  The signature is specified to explicitly identify types.
The |runTypeof| function is a utilty function that evaluates the
|Reader| monad.  The initial environment is empty because there are no
predefined symbols in our language.  |runTypeof| should be used to
integrate the type checker with other language elements.

\begin{code}
  typeof :: TmLang -> TyMonad
  typeof = cata

  runTypeof t = (runReaderT (typeof t) [])
\end{code}

