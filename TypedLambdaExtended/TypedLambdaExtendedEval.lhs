\section{Evaluation}

\begin{code}
  module TypedLambdaExtendedEval where

  import LangUtils
  import TypedLambdaExtendedEnv
  import TypedLambdaExtendedAST
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
      | LambdaVal (TmValEnv -> TmValEnv)
      | TmTupleVal [TmVal]

  instance Show TmVal where
      show (TmBoolVal x) = show x
      show (TmIntVal x) = show x
      show (LambdaVal x) = "<Lambda Value>"
      show (TmTupleVal vs) = show vs

  instance Subtype Bool TmVal where
      inj x = (TmBoolVal x)
      prj (TmBoolVal x) = Just x
      prj (TmIntVal _) = Nothing
      prj (LambdaVal _) = Nothing
      prj (TmTupleVal _) = Nothing

  instance Subtype Int TmVal where
      inj x = (TmIntVal x)
      prj (TmBoolVal _) = Nothing
      prj (TmIntVal x) = Just x
      prj (LambdaVal _) = Nothing
      prj (TmTupleVal _) = Nothing

  instance Subtype (TmValEnv->TmValEnv) TmVal where
      inj x = (LambdaVal x)
      prj (TmBoolVal _) = Nothing
      prj (TmIntVal _) = Nothing
      prj (LambdaVal x) = Just x
      prj (TmTupleVal _) = Nothing

  instance Subtype [TmVal] TmVal where
      inj x = (TmTupleVal x)
      prj (TmBoolVal _) = Nothing
      prj (TmIntVal _) = Nothing
      prj (LambdaVal _) = Nothing
      prj (TmTupleVal x) = Just x

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

  instance Algebra TmInt TmValEnv where
      phi (TmConstInt x) = return $ inj x

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

  instance Algebra TmIf TmValEnv where
      phi (If b t e) =
          do { b' <- b
             ; case (prj b') of
               Just b'' -> if b'' then t else e
               Nothing -> error ((show b') ++ " is not boolean")
             }

  instance Algebra TmVar TmValEnv where
      phi (TmVar v) = do { val <- asks (lookup v)
                         ; case val of
                           Just x -> return x
                           Nothing -> error ("Variable " ++ (v ++ " not found"))
                         }

  instance Algebra TmTuple TmValEnv where
      phi (TmTuple vs) = do { vs' <- sequence vs
			    ; return $ inj vs'
			    }

      phi (TmPrj i t) = do { t' <- t
			   ; case (prj t') of
			     (Just (TmTupleVal vs)) -> return $ inj (vs!!i)
			     Nothing -> error "Bad tuple value"
			   }

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
