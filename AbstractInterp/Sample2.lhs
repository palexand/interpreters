\documentclass[10pt]{article}

\usepackage{url}

\parskip=\medskipamount
\parindent=0pt

%include lhs2tex.fmt
%include LangUtils.fmt
%include lhs2tex.sty

\title{InterpreterLib Examples}
\author{Uk'taad B'mal \\
  Information and Telecommunications Technology Center \\
  The University of Kansas \\
  2335 Irving Hill Rd, Lawrence, KS 66045}

\begin{document}

\maketitle

\begin{abstract}
  The purpose of this document is to provide an annoted example using
  the |InterprterLib| package to define simple interpreters using the
  built-in abstract syntax definitions.  We also extend the language
  to contain basic containers -- sets, bags and sequences -- in
  addition to the integer types used in previous interpreters.  This
  document assumes some knowledge of modular interpreter construction
  and refers to earlier interpreters written by the Uk'taad B'mal
  group.  These are available upon request.
\end{abstract}

\begin{code}
{-# OPTIONS -fglasgow-exts -fno-monomorphism-restriction #-}
  import InterpreterLib.Algebras
  import InterpreterLib.Functors
  import InterpreterLib.Terms.ArithTerm
  import InterpreterLib.Terms.IfTerm
  import InterpreterLib.Terms.LambdaTerm
  import InterpreterLib.Terms.VarTerm
  import InterpreterLib.Terms.ContainerTerm
  import Data.Ratio
  import Monad
  import Control.Monad.Reader
\end{code}

\section{Example Language}

Define the value space and give it some nice mathematical properties.
Unfortunately, this is about the hardest thing there is to do for this
interpreter definition.  

The basic value set for this new language includes numbers, booleans,
functions, sets, sequences and bags.  In addition to defining the data
type, we also make the values instances of |Eq| and |Show| to make
them easier to use.

\begin{code}
  data Value 
      = ValNum Int
      | ValBool Bool
      | ValLambda (ValueMonad -> ValueMonad)
      | ValSet [Value]
      | ValSeq [Value]
      | ValBag [Value]

  instance Show Value where
      show (ValNum x) = show x
      show (ValBool x) = show x
      show (ValLambda x) = show x
      show (ValSet x) = "{" ++ show x ++ "}"
      show (ValSeq x) = "<" ++ show x ++ ">"
      show (ValBag x) = "{*" ++ show x ++ "*}"

  instance Eq Value where
      (==) (ValNum x) (ValNum y) = x == y
      (==) (ValBool x) (ValBool y) = x == y
      (==) (ValLambda x) (ValLambda y) = x == y
      (==) (ValSet x) (ValSet y) = x == y
      (==) (ValSeq x) (ValSeq y) = x == y
      (==) (ValBag x) (ValBag y) = x == y
\end{code}

With values defined, now defin an environment and support functions
for lambda.

\end{code}
  type Env = [(String,Value)]

  addSubst :: (String,Value) -> Env -> Env
  addSubst v env = v:env

  findValue s env = case lookup s env of
                    (Just x) -> x
                    Nothing -> error "Variable not in scope"

  type ValueMonad = Reader Env Value

  instance Show (ValueMonad -> ValueMonad) where
      show f = "<Function Value>"

  instance Eq (ValueMonad -> ValueMonad) where
      (==) _ _ = error "Cannot compare function values."
\end{code}

Define numeric properties over value so we can use +,-,*,/ and friends
rather than define a call helper functions everywhere.  This is a big
enough pain that helpers might be preferable.

\begin{code}
  instance Num Value where
      (+) (ValNum x) (ValNum y) = ValNum (x+y)
      (+) (ValBool x) (ValBool y) = ValBool (x || y)
      (-) (ValNum x) (ValNum y) = ValNum (x-y)
      (*) (ValNum x) (ValNum y) = ValNum (x*y)
      (*) (ValBool x) (ValBool y) = ValBool (x && y)
      negate (ValNum x) = ValNum (-x)
      negate (ValBool x) = ValBool (not x)
      abs (ValNum x) = ValNum (abs x)
      signum (ValNum x) = ValNum (signum x)
      fromInteger = ValNum . fromInteger

  instance Enum Value where
      succ (ValNum x) = ValNum (x+1)
      pred (ValNum x) = ValNum (x-1)
      toEnum = ValNum
      fromEnum (ValNum x) = x

  instance Integral Value where
      (div) (ValNum x) (ValNum y) = ValNum (div x y)
      quotRem (ValNum x) (ValNum y) = case (quotRem x y) of
                                      (x,y) -> (ValNum x, ValNum y)
      toInteger (ValNum x) = toInteger x

  instance Real Value where
      toRational (ValNum x) = (toInteger x) % (toInteger 1)

  instance Ord Value where
      (<) (ValNum x) (ValNum y) = x < y
      (>=) (ValNum x) (ValNum y) = x >= y
      (>) (ValNum x) (ValNum y) = x > y
      (<=) (ValNum x) (ValNum y) = x <= y
      max (ValNum x) (ValNum y) = ValNum (max x y)
      min (ValNum x) (ValNum y) = ValNum (min x y)
\end{code}

Define helper functions for creating interpreter.  First, define the
functions for the semantices of various operations.

\begin{code}
  phiArith (Add x1 x2) = liftM2 (+) x1 x2
  phiArith (Sub x1 x2) = liftM2 (-) x1 x2
  phiArith (Mult x1 x2) = liftM2 (*) x1 x2
  phiArith (Div x1 x2) = liftM2 (div) x1 x2
  phiArith (NumEq x1 x2) = liftM2 (\v1 -> \v2 -> if (v1==v2) then 1 else 0) x1 x2
  phiArith (Num x1) = return (ValNum x1)

  phiIf (IfTerm x1 x2 x3) = do { (ValBool x1') <- x1
                               ; liftM id (if x1' then x2 else x3)
                               }
  phiIf TrueTerm = return $ ValBool True
  phiIf FalseTerm = return $ ValBool False

  phiLambda (Lam s _ t)
      = return $ ValLambda (\v -> do { env <- ask
                                     ; v' <- v
                                     ; t' <- local (const (addSubst (s,v') env)) t
                                     ; return t'
                                     })

  phiLambda (App t1 t2)
      = do { (ValLambda f) <- t1
           ; (f t2)
           }

  phiVar :: (VarTerm ValueMonad) -> ValueMonad
  phiVar (VarTerm s)
      = do { v <- asks (findValue s)
           ; return v
           }
  -- The dummy term definition helps Haskell figure out what type VarTerm
  -- is defined over.  It is never used.
  phiVar (DummyTerm x)
      = return (ValBool True)
\end{code}

\begin{code}
  phiCont (Set _ s)
      = do { vals <- sequence s
           ; return $ ValSet vals
           }
  phiCont (Seq _ s)
      = do { vals <- sequence s
           ; return $ ValSeq vals
           }
  phiCont (Bag _ s)
      = do { vals <- sequence s
           ; return $ ValBag vals
           }
  phiCont (InTerm v s)
      = do { v' <- v
           ; s' <- s
           ; return $ case s' of
                              (ValSet t) -> (ValBool (elem v' t))
                              (ValSeq t) -> (ValBool (elem v' t))
                              (ValBag t) -> (ValBool (elem v' t))
           }
  phiCont (SubTerm s1 s2)
      = do { s1' <- s1
           ; s2' <- s2
           ; return $ case s1' of
                        (ValSet t1) -> case s2' of
                                       (ValSet t2) -> (ValBool (and (map (\x -> elem x t2) t1)))
                                       _ -> error "Cannot find subset relatiionship"
                        (ValSeq t1) -> case s2' of
                                       (ValSeq t2) -> error "Subsequence not implemented"
                                       _ -> error "Cannot find subsequence relationship"
                        (ValBag t1) -> case s2' of
                                       (ValBag t2) -> error "Sub-bag not implemented"
                                       _ -> error "Cannot fund sub-bag relationship"
           }
  phiCont (EqTerm s1 s2)
      = do { s1' <- s1
           ; s2' <- s2
           ; return $ case s1' of
                             (ValSet t1) -> 
                                 case s2' of
                                          (ValSet t2) -> (ValBool ((and (map (\x -> elem x t2) t1)) && (and (map (\x -> elem x t1) t2))))
                                          _ -> error "Cannot perform set comparison with non-set"
                             (ValBag t1) -> error "Bag equal not implemented"
                             (ValSeq t1) ->
                                 case s2' of
                                          (ValSeq t2) -> (ValBool (t1 == t2))
                                          _ -> error "Cannot perform sequence comparison with non-sequence"
           }
\end{code}

\section{Term Type and Term Language}

Build the term type and the term language by composing the term types
and finding the fixed point.

\begin{code}
  type TermType
      = ArithTerm
      :$: IfTerm
      :$: LambdaTerm ()
      :$: VarTerm
      :$: ContainerTerm ()

  type TermLang = Fix TermType
\end{code}

\section{Explicit Algebra}

Build the explicit algebra by composing algebras for each language
term type.

\begin{code}
  termAlgebra
      = (mkAlgebra phiArith)
      @+@ (mkAlgebra phiIf)
      @+@ (mkAlgebra phiLambda)
      @+@ (mkAlgebra phiVar)
      @+@ (mkAlgebra phiCont)
\end{code}

\section{Test Cases}

Here are several test terms and an evaluation function to play with.  To
evaluate a term, use the following:

\begin{spec}
  runReader (eval termX) []
\end{spec}

|(eval termX)| returns the monad resulting from evaluating |termX|.
|runReader| then evaluates that monad with the empty environment,
|[]|.  In the past, we have encapsulated the environment in some kind
of constructed type.  Here, we are just using a list.

\begin{code}
  -- Shorthand eval function
  eval = (cata termAlgebra)

  -- Shorthans for (S (Right x)) and (S (Right y))
  sright = S . Right

  sleft = S . Left

  -- Some simple arithmetic terms
  term1 = (inn (sleft (Num 1)))

  term2 = (inn (sleft (Add term1 term1)))

  term3 = (inn (sleft (Mult term2 term2)))

  term4 = (inn (sleft (Sub term2 term1)))

  -- Shorthands for true and false values
  termTrue = (inn (sright (sleft TrueTerm)))

  termFalse = (inn (sright (sleft FalseTerm)))

  -- if term that returns true case
  term5 = (inn (sright (sleft (IfTerm termTrue term4 term3))))

  -- if term that returns false case
  term6 = (inn (sright (sleft (IfTerm termFalse term4 term3))))

  -- Constant function that always returns term6
  term7 = (inn (sright (sright (sleft (Lam "x" () term6)))))

  -- Constant function applied to 1
  term8 = (inn (sright (sright (sleft (App term7 term1)))))

  -- Variable x
  term9 = (inn (sright (sright (sright (sleft (VarTerm "x"))))))

  -- Identity function
  term10 = (inn (sright (sright (sleft (Lam "x" () term9)))))

  -- Identity function applied to 1
  term11 = (inn (sright (sright (sleft (App term10 term1)))))

  -- Simple set of integers
  term12 = (inn (sright (sright (sright (sright (Set () [(inn (sleft (Num 1)))]))))))

  -- Simple sequence of integers
  term13 = (inn (sright (sright (sright (sright (Seq () [(inn (sleft (Num 1)))]))))))

  -- Simple bag of integers
  term14 = (inn (sright (sright (sright (sright (Bag () [(inn (sleft (Num 1)))]))))))

  -- Simple set of set of integers
  term15 = (inn (sright (sright (sright (sright (Set () [term12]))))))

  -- The empty set
  emptySet = (inn (sright (sright (sright (sright (Set () []))))))

  -- Set is subset of itself
  term16 = (inn (sright (sright (sright (sright (SubTerm term12 term12))))))

  -- Set is not subset of empty set
  term17 = (inn (sright (sright (sright (sright (SubTerm term12 emptySet))))))

  -- Empty set is subset of any set
  term18 = (inn (sright (sright (sright (sright (SubTerm emptySet term12))))))

  -- Set comparison
  term19 = (inn (sright (sright (sright (sright (Set () [(inn (sleft (Num 1))),(inn (sleft (Num 2)))]))))))

  term20 = (inn (sright (sright (sright (sright (Set () [(inn (sleft (Num 2))),(inn (sleft (Num 1)))]))))))

  term21 = (inn (sright (sright (sright (sright (EqTerm term19 term20))))))

  term22 = (inn (sright (sright (sright (sright (EqTerm emptySet term20))))))

  -- Sequence comparison

  term23 = (inn (sright (sright (sright (sright (Seq () [(inn (sleft (Num 1))),(inn (sleft (Num 2)))]))))))

  term24 = (inn (sright (sright (sright (sright (Seq () [(inn (sleft (Num 2))),(inn (sleft (Num 1)))]))))))

  term25 = (inn (sright (sright (sright (sright (EqTerm term23 term24))))))

  term26 = (inn (sright (sright (sright (sright (EqTerm term23 term23))))))
\end{code}

\end{document}
