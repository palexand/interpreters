\section{Evaluation}

\begin{code}
  module SystemFEval where

  import LangUtils
  import SystemFEnv
  import SystemFAST
  import Control.Monad.Reader
  import Control.Monad.Error
\end{code}
  
\subsection{Value Representation}

There are three values associated with the Lambda language that all
interpretable functions must converge to - booleans, integers, and
lambda values.  Together, these are specified in the |TmVal|
constructed type.  Note that this type is recursive, unlike the term
language and type language specifications.  The \texttt{Haskell} types
used to represent primitive values are defined to be subtypes of the
aggregate \texttt{TmVal} type.  Thus, |prj| and |inj| are define
between types.

\begin{code}
  data TmVal
      = TmBoolVal Bool
      | TmIntVal Int
      | TmUnitVal
      | LambdaVal (TmValEnv -> TmValEnv)
      | TmTupleVal [TmVal]
      | TmLVal TmVal
      | TmRVal TmVal

  instance Show TmVal where
      show (TmBoolVal x) = show x
      show (TmIntVal x) = show x
      show TmUnitVal = "()"
      show (LambdaVal x) = "<Lambda Value>"
      show (TmTupleVal vs) = show vs
      show (TmLVal v) = "(L " ++ (show v) ++ ")"
      show (TmRVal v) = "(R " ++ (show v) ++ ")"

  instance Subtype Bool TmVal where
      inj x = (TmBoolVal x)
      prj (TmBoolVal x) = Just x
      prj (TmIntVal _) = Nothing
      prj (LambdaVal _) = Nothing
      prj (TmTupleVal _) = Nothing
      prj TmUnitVal = Nothing
      prj (TmLVal _) = Nothing
      prj (TmRVal _) = Nothing

  instance Subtype Int TmVal where
      inj x = (TmIntVal x)
      prj (TmBoolVal _) = Nothing
      prj (TmIntVal x) = Just x
      prj (LambdaVal _) = Nothing
      prj (TmTupleVal _) = Nothing
      prj TmUnitVal = Nothing
      prj (TmLVal _) = Nothing
      prj (TmRVal _) = Nothing

  instance Subtype (TmValEnv->TmValEnv) TmVal where
      inj x = (LambdaVal x)
      prj (TmBoolVal _) = Nothing
      prj (TmIntVal _) = Nothing
      prj (LambdaVal x) = Just x
      prj (TmTupleVal _) = Nothing
      prj TmUnitVal = Nothing
      prj (TmLVal _) = Nothing
      prj (TmRVal _) = Nothing

  instance Subtype [TmVal] TmVal where
      inj x = (TmTupleVal x)
      prj (TmBoolVal _) = Nothing
      prj (TmIntVal _) = Nothing
      prj (LambdaVal _) = Nothing
      prj (TmTupleVal x) = Just x
      prj TmUnitVal = Nothing
      prj (TmLVal _) = Nothing
      prj (TmRVal _) = Nothing

  instance Subtype (Either TmVal TmVal) TmVal where
      inj (Right x) = TmRVal x
      inj (Left x) = TmLVal x
      prj (TmBoolVal _) = Nothing
      prj (TmIntVal _) = Nothing
      prj (LambdaVal _) = Nothing
      prj (TmTupleVal _) = Nothing
      prj TmUnitVal = Nothing
      prj (TmLVal x) = Just (Left x)
      prj (TmRVal x) = Just (Right x)

				   

  type Env = Environment TmVal
\end{code}

\subsection{The Evaluator Monad}

The monad used to support evaluation is a composition of the
|ErrorMonad| and the |Reader| monad with the |ErrorMondad|
encapsulated by the |Reader|.

\begin{code}
  data TmError = Err String deriving (Show,Eq)

  instance Error TmError where
      noMsg = Err "Type Error"
      strMsg s = Err s

  type TmValEnv = ReaderT Env (Either TmError) TmVal
\end{code}

\subsection{Expressions as Algebras}

\begin{code}
  instance Algebra TmBool TmValEnv where
      phi TmTrue = return $ inj True
      phi TmFalse = return $ inj False

  mkTrue = toTmLang TmTrue
  mkFalse = toTmLang TmFalse

  instance Algebra TmInt TmValEnv where
      phi (TmConstInt x) = return $ inj x

  mkInt x = toTmLang $ TmConstInt x

  instance Algebra TmUnit TmValEnv where
      phi TmUnit = return TmUnitVal

  instance Algebra TmOp TmValEnv where
      phi (TmAdd x y) = 
          do { x' <- x
             ; y' <- y
             ; case (prj x') of
               Just x'' -> case (prj y') of
                           Just y'' -> return $ inj ((x''::Int)+(y''::Int))
                           Nothing -> error ((show y') ++ " not an integer")
               Nothing -> error ((show x') ++ " not an integer")
             }

      phi (TmSub x y) = 
          do { x' <- x
             ; y' <- y
             ; case (prj x') of
               Just x'' -> case (prj y') of
                          Just y'' -> return $ inj ((x''::Int)-(y''::Int))
                          Nothing -> error ((show y') ++ " not an integer")
               Nothing -> error ((show x') ++ " not an integer")
             }

      phi (TmMul x y) = 
          do { x' <- x
             ; y' <- y
             ; case (prj x') of
               Just x'' -> case (prj y') of
                          Just y'' -> return $ inj ((x''::Int)*(y''::Int))
                          Nothing -> error ((show y') ++ " not an integer")
               Nothing -> error ((show x') ++ " not an integer")
             }

      phi (TmDiv x y) = 
          do { x' <- x
             ; y' <- y
             ; case (prj x') of
               Just x'' -> case (prj y') of
                          Just y'' -> if (y''::Int) == 0
	                              then throwError $ Err "Division by zero"
	                              else return $ inj ((x''::Int) `div` (y''::Int))
                          Nothing -> error ((show y') ++ " not an integer")
               Nothing -> error ((show x') ++ " not an integer")
             }

  mkAdd x y = toTmLang $ TmAdd x y
  mkSub x y = toTmLang $ TmSub x y
  mkMul x y = toTmLang $ TmMul x y
  mkDiv x y = toTmLang $ TmDiv x y

  instance Algebra TmIf TmValEnv where
      phi (If b t e) =
          do { b' <- b
             ; case (prj b') of
               Just b'' -> if b'' then t else e
               Nothing -> error ((show b') ++ " is not boolean")
             }

  mkIf a b c = toTmLang $ If a b c

  instance Algebra TmVar TmValEnv where
      phi (TmVar v) = do { val <- asks (lookup v)
                         ; case val of
                           Just x -> return x
                           Nothing -> error ("Variable " ++ (v ++ " not found"))
                         }

      phi (TmTVar v) = do { val <- asks (lookup v)
			  ; case val of
			    Just x -> return x
			    Nothing -> error ("Type variable " ++ (v ++ " not found"))
			  }

  mkVar s = toTmLang $ TmVar s
  mkTVar s = toTmLang $ TmTVar s

  instance Algebra TmTuple TmValEnv where
      phi (TmTuple vs) = do { vs' <- sequence vs
			    ; return $ inj vs'
			    }

      phi (TmPrj i t) = do { t' <- t
			   ; case (prj t') of
			     (Just (TmTupleVal vs)) -> return $ inj (vs!!i)
			     Nothing -> error "Bad tuple value"
			   }

  mkTuple vs = toTmLang $ TmTuple vs
  mkTmPrj i t = toTmLang $ TmPrj i t

  instance Algebra TmSum TmValEnv where
      phi (InL te _) = do { te' <- te
			; return $ inj $ (TmLVal te')
			}

      phi (InR te _) = do { te' <- te
			; return $ inj $ (TmRVal te')
			}

      phi (TmCase tc _ lv lt rv rt) = do { gamma <- ask
					 ; tc' <- tc
					 ; case (prj tc') of
					   (Just (TmLVal v)) ->
					     local (const ((lv,v):gamma)) lt
					   (Just (TmRVal v)) ->
					     local (const ((rv,v):gamma)) rt
					   _ -> throwError $ Err "Case argument is not a Sum type."
					 }

  mkLVal x t = toTmLang $ InL x t
  mkRVal x t = toTmLang $ InR x t
  mkCase c t sl vl sr vr = toTmLang $ TmCase c t sl vl sr vr

  instance Algebra TmFn TmValEnv where
      phi (TmLambda s ty te) =
          do { env <- ask
             ; return $ inj $ (\v -> do { v' <- v
                                        ; local (const ((s,v'):env)) te
                                        })
             }

      phi (TmApp te1 te2) =
          do { te1' <- te1
             ; case (prj te1') of 
               (Just (LambdaVal f)) -> (f te2)
               a -> error ((show a) ++ " is not a lambda value")
             }

      phi (TmTLambda s te) =
	  do { te' <- te
	     ; return $ inj te'
	     }

      phi (TmTApp te1 te2) =
	  do { te1' <- te1
	     ; return $ inj $ te1'
	     }

  mkLambda s ty te = toTmLang $ TmLambda s ty te
  mkApp t1 t2 = toTmLang $ TmApp t1 t2
  mkTLambda s te = toTmLang $ TmTLambda s te
  mkTApp t1 t2 = toTmLang $ TmTApp t1 t2
\end{code}

The |eval| function generates a monad from a term language element.
The monad is an |ErrorMonad| composed with a |Reader| monad, thus the
result of applying |runReader| is either a value or an error message.
|runEval| applies |runReaderT| to the |Reader| monad resulting from
|eval| on an environment parameter.  |execute| applies |runEval| with
an empty environment.

\begin{code}
  eval :: TmLang -> TmValEnv
  eval = cata
 
  runEval t e = (runReaderT (eval t) e)

  execute t = runEval t []
\end{code}
