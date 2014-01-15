\documentclass[10pt]{article}

\usepackage{haskell}
\usepackage{proof}
\usepackage{fullpage}

\parskip=\medskipamount
\parindent=0pt

\bibliographystyle{plain}

\newcommand{\isa}{\ensuremath{\; {:}{:}{=} \;}}
\newcommand{\ora}{\ensuremath{\;\mid\;}}

\title{EECS 762 - Project 1 Solution}
\author{Perry Alexander \\
  The University of Kansas EECS Department\\
  \texttt{alex@ittc.ku.edu}}

\begin{document}

\maketitle

\section{Introduction}

The objective of Project 1 is to write an interpreter for untyped
calculus expressions as defined in \emph{Types and Programming
Languages}~\cite{Pie02a}, Chapter 5, Figure 5-3.  This language,
called, $\lambda$, is the most basic form of the lambda calculus.  The
definition of the abstract syntax provides the following two forms for
terms and values:

\begin{eqnarray*}
  t & \isa & x \ora \lambda x.t \ora t\; t \\
  v & \isa & \lambda x.t \\
\end{eqnarray*}

The definition for call-by-value evaluation provides the following
three evaluation rules:

\[\vcenter{\infer[\textsc{E-App1}]{t_1 t_2 \longrightarrow t_1^{'} t_2}{t_1\longrightarrow t_1^{'}}}\]

\[\vcenter{\infer[\textsc{E-App2}]{t_1 t_2 \longrightarrow t_1 t_2^{'}}{t_2\longrightarrow t_2^{'}}}\]

\[\vcenter{\infer[\textsc{E-AppAbs}]{(\lambda x.t_{12})v_2 \longrightarrow [x\rightarrow v_2]t_{12}}{}}\]

Our objective is to: (i) define a data structure for representing
$\lambda$ terms embodying the abstract syntax; and (ii) an evaluation
function embodying the evaluation rules for call-by-value and
call-by-name interpretation.

\section{UntypedLambda Module}

The \texttt{UntypedLambdaMonad} module provides a capability for evaluating expressions.

\begin{code}
  module UntypedLambdaMonad where

  import LangUtils
  import Monad
  import Control.Monad.Reader
  import Control.Monad.Error
\end{code}

\begin{code}
\end{code}

\begin{code}
  data TmVar a = TmVar String deriving (Show,Eq)

  instance Functor TmVar where
      fmap f (TmVar x) = (TmVar x)

  data TmFn a = TmLambda String a | TmApp a a deriving (Show,Eq)

  instance Functor TmFn where
      fmap f (TmLambda s t) = (TmLambda s (f t))
      fmap f (TmApp s t) = (TmApp (f s) (f t))

  type TmLangSum = (Sum TmVar TmFn)

  type TmLang = Rec TmLangSum

  instance Show TmLang where
      show x = show (unS (out x))    

  toTmLang :: (Subsum f TmLangSum) => f TmLang -> TmLang
  toTmLang = toSum
\end{code}

\begin{code}
  data TmVal = LambdaVal (TmMonad -> TmMonad)

  type TmMonad = ReaderT [(String,TmVal)] (Either String) TmVal
\end{code}

\begin{code}
  instance Algebra TmVar TmMonad where
      phi (TmVar v) = do { val <- asks (lookup v)
			 ; case val of
			   (Just x) -> return $ inj x
			   Nothing ->
			     throwError ("Variable " ++ (v ++ " not found"))
			 }

  instance Algebra TmFn TmMonad where
      phi l@(TmLambda s t) = do { env <- ask
				; return $ LambdaVal (\v -> do { v' <-v
							       ; (local (const ((s,v'):env)) t)
							     })
			      }

      phi (TmApp t1 t2) = do { t1' <- t1
			     ; case t1' of
			       (LambdaVal f) -> (f t2)
			       _ -> throwError "Not a Lambda"
			     }
			       
\end{code}

\begin{code}
  eval :: TmLang -> TmMonad
  eval = cata

  runEval t e = (runReaderT (eval t) e)

  interpret t = runEval t []
\end{code}

\begin{code}
  ident :: TmLang
  ident = toTmLang (TmLambda "x" (toTmLang (TmVar "x")))

  tru :: TmLang
  tru = toTmLang (TmLambda "t" (toTmLang (TmLambda "f" (toTmLang (TmVar "t")))))
  fls :: TmLang
  fls = toTmLang (TmLambda "t" (toTmLang (TmLambda "f" (toTmLang (TmVar "f")))))

  c0 :: TmLang
  c0 = toTmLang (TmLambda "t" (toTmLang (TmLambda "f" (toTmLang (TmVar "f")))))

  c1 :: TmLang
  c1 = toTmLang (TmLambda "x" (toTmLang (TmLambda "y" (toTmLang (TmApp (toTmLang (TmVar "x")) (toTmLang (TmVar "y")))))))

  c2 :: TmLang
  c2 = toTmLang (TmLambda "x"
	(toTmLang (TmLambda "y"
	 (toTmLang (TmApp (toTmLang (TmVar "x")) (toTmLang (TmApp (toTmLang (TmVar "x")) (toTmLang (TmVar "y")))))))))

  scc :: TmLang
  scc = (toTmLang (TmLambda "x"
         (toTmLang (TmLambda "y"
	  (toTmLang (TmLambda "z"
	   (toTmLang (TmApp (toTmLang (TmVar "y"))
            (toTmLang (TmApp (toTmLang (TmVar "x"))
             (toTmLang (TmApp (toTmLang (TmVar "y")) (toTmLang (TmVar "z"))))))))))))))

  omega :: TmLang
  omega = (toTmLang (TmLambda "x"
	   (toTmLang (TmApp (toTmLang (TmVar "x")) (toTmLang (TmVar "x"))))))

  testTru :: TmLang
  testTru = (toTmLang (TmApp (toTmLang (TmApp tru ident)) fls))

  testFls :: TmLang
  testFls = (toTmLang (TmApp (toTmLang (TmApp fls fls)) ident))

  testCase1 :: TmLang
  testCase1 = (toTmLang (TmApp (toTmLang (TmApp tru ident)) (toTmLang (TmApp omega omega))))

\end{code}

\bibliography{types,prog-langs}

\end{document}
