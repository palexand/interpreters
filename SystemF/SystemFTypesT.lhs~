\section{Type Checking}

\subsection{Type Values}

These are the type values available in our language.  For the type
language, this will serve as the carrier set or value space for both
the type langauge and the term language under type checking.  |phi|
for the type language is defined over |Ty a| while |phi| for the term
language type checker is defined over |Tm a|.  In effect, |phi|
evaluates the term language to a type value rather than a term value.

\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
module SystemFTypesT where
  import LangUtils
  import SystemFAST
  import SystemFEnv
  import Monad
  import Control.Monad.Error
  import Control.Monad.Reader
\end{code}

\begin{code}
  type TyMonFn = TyMonad -> TyMonad

  instance Show TyMonFn where
      show t = "<Monad Function>"

  instance Eq TyMonFn where
      x == y = False

  data TyVal
      = TyAbsVal TyVal TyVal
      | TyBoolVal
      | TyIntVal
      | TyUnitVal
      | TyTupleVal [TyVal]
      | TyAllVal (TyMonad -> TyMonad)
        -- Existential needs to be reviewed and updated
      | TyExistVal TyVal TyVal
      | TyVarVal String
      | TySumVal TyVal TyVal
	deriving (Show,Eq)
\end{code}

\begin{code}
  class SubType a b where
      (<<<) :: a -> b -> Bool

  -- Must update for existentials
  instance SubType TyVal TyVal where
      TyUnitVal <<< TyUnitVal = True
      TyIntVal <<< TyIntVal = True
      TyBoolVal <<< TyBoolVal = True
      (TyAbsVal d1 r1) <<< (TyAbsVal d2 r2) = (d2 <<< d1) && (r1 <<< r2)
      (TyTupleVal vs1) <<< (TyTupleVal vs2) =
	  (zip vs1 vs2) == [(x,y) | (x,y) <- (zip vs1 vs2), x <<< y]
      _ <<< _ = False
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
      phi TyBool = return $ inj TyBoolVal
      phi TyInt = return $ inj TyIntVal

  mkBool = toTyLang TyBool
  mkInt = toTyLang TyInt
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
                          ; return $ inj (TyAbsVal x' y')
                          }

  mkTyAbs x y = toTyLang ( x :->: y )
\end{code}

\subsubsection{Tuple Type}

\begin{code}
  instance Functor TyTuple where
      fmap f (TyTuple ts) = TyTuple (map f ts)

  instance Algebra TyTuple TyMonad where
      phi (TyTuple s) = do { s' <- sequence s
			   ; return $ inj $ (TyTupleVal s')
			   }

  mkTupleTy ts = toTyLang $ TyTuple ts
\end{code}

\subsubsection{Sum Types}

\begin{code}
  instance Functor TySum where
      fmap f (x :+: y) = (f x) :+: (f y)

  instance Algebra TySum TyMonad where
      phi (x :+: y) = do { x' <- x
			 ; y' <- y
			 ; return $ inj (TySumVal x' y')
			 }
  mkSumTy x y = toTyLang $ (x :+: y)

\end{code}

\subsubsection{Type Variables}

\begin{code}
  instance Functor TyVar where
      fmap f (TyVar x) = (TyVar x)

  instance Algebra TyVar TyMonad where
      phi (TyVar x) = do { val <- asks (lookup x)
			 ; case val of
			   Just x -> return $ inj x
			   Nothing -> throwError $ Err "Type Variable not found"
			 }

  mkTyVar s = toTyLang $ TyVar s
\end{code}

The |evalTy| function is a separate function for evaluating elements
of the type language.

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
      phi TmTrue = return $ inj TyBoolVal
      phi TmFalse = return $ inj $ TyBoolVal

  instance Algebra TmInt TyMonad where
      phi (TmConstInt x) = return $ inj TyIntVal

  instance Algebra TmUnit TyMonad where
      phi TmUnit = return $ inj $ TyUnitVal

  instance Algebra TmTuple TyMonad where
      phi (TmTuple xs) = do { xs' <- sequence xs
			    ; return $ inj (TyTupleVal xs')
			    }

      phi (TmPrj i t) = do { t' <- t
			   ; case t' of
			     (TyTupleVal tys) -> 
			       if ((i >= 0) && (i < (length tys)))
			       then return (inj (tys!!i))
			       else throwError $ Err "Tuple index out of range"
			     _ -> throwError $ Err "Project argument not a tuple type"
			   }

  instance Algebra TmSum TyMonad where
      phi (InL te ty) = do { ty' <- evalTy ty
			   ; case ty' of
			     (TySumVal x y) -> return $ inj (TySumVal x y)
			     _ -> throwError $ Err "Type must be a sum"
			   }

      phi (InR te ty) = do { ty' <- evalTy ty
			   ; case ty' of
			     (TySumVal x y) -> return $ inj (TySumVal x y)
			     _ -> throwError $ Err "Type must be a sum"
			   }

      phi (TmCase v ty lt lv rt rv) = do { lv' <- lv
					 ; rv' <- rv
					 ; if lv'==rv'
					   then return $ inj lv'
					   else throwError $ Err "Case expressions must be of the same type."
					 }

  instance Algebra TmOp TyMonad where
      phi (TmAdd x y) = do { x' <- x
                           ; y' <- y
                           ; if (x' <<< TyIntVal &&
                                 y' <<< TyIntVal)
                             then return $ inj TyIntVal
                             else throwError $ Err "Argument to Add not Integer"
                           }

      phi (TmSub x y) = do { x' <- x
                           ; y' <- y
                           ; if (x' <<< TyIntVal &&
                                 y' <<< TyIntVal)
                             then return $ inj TyIntVal
                             else throwError $ Err "Argument to Sub not Integer"
                           }

      phi (TmMul x y) = do { x' <- x
                           ; y' <- y
                           ; if (x' <<< TyIntVal &&
                                 y' <<< TyIntVal)
                             then return $ inj TyIntVal
                             else throwError $ Err "Argument to Mul not Integer"
                           }
      phi (TmDiv x y) = do { x' <- x
                           ; y' <- y
                           ; if (x' <<< TyIntVal &&
                                 y' <<< TyIntVal)
                             then return $ inj TyIntVal
                             else throwError $ Err "Argument to Div not Integer"
                           }


  instance Algebra TmIf TyMonad where
      phi (If c t e) = do { c' <- c
                          ; t' <- t
                          ; e' <- e
                          ; if (c' == TyBoolVal &&
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
                                  ; return $ inj (TyAbsVal ty' te')
                                  }

      phi (TmApp te1 te2) = do { te1' <- te1
                               ; te2' <- te2
                               ; checkLambda te1' te2'
                               }
            where checkLambda l te2 =
		      case l of
			     (TyAbsVal tty tte) -> if tty <<< te2
						   then return $ inj tte 
						   else throwError $ Err "Actual parameter type is not a subtype of formal parameter type"
		             _ -> throwError $ Err "First argument to application must be a Lambda"

      phi (TmTLambda s te) = do { gamma <- ask
				; return $ inj $ TyAllVal
				  (\tv -> do { tv' <- tv
					     ; local (const (addBinding gamma (s,tv'))) te
					     })
				}

      phi (TmTApp te ty) = do { te' <- te
			      ; case te' of
				(TyAllVal f) -> (f (evalTy ty))
				_ -> throwError $ Err "Not a universal"
			      }

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

