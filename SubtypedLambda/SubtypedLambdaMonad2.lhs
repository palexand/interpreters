\documentclass[10pt]{article}

%include /home/alex/tmp/Literate/lhs2TeX.fmt
%include /home/alex/tmp/Literate/lhs2TeX.sty

%format s11 = "\Varid{s}_{11}"
%format s12 = "\Varid{s}_{12}"
%format s21 = "\Varid{s}_{21}"
%format s12 = "\Varid{s}_{22}"
%format s1 = "\Varid{s}_1"
%format s2 = "\Varid{s}_2"
%format t1 = "\Varid{t}_1"
%format t2 = "\Varid{t}_2"
%format t3 = "\Varid{t}_3"
%format c0 = "\Varid{c}_0"
%format c1 = "\Varid{c}_1"
%format c2 = "\Varid{c}_2"
%format tt1 = "\Varid{tt}_1"
%format tt2 = "\Varid{tt}_2"
%format dtt1 = "\Varid{dtt}_1"
%format Ctx = "\Varid{\Gamma}"
%format ctx = "\Varid{\gamma}"
%format Ty = "\Varid{T}"
%format eval = "\Varid{eval}_{<:}"
%format typeof = "\Varid{typeof}_{<:}"
%format omega = "\Varid{\Omega}"

%\usepackage{haskell}

\usepackage{proof}
\usepackage{fullpage}

\bibliographystyle{plain}

\parskip=\medskipamount
\parindent=0pt

\newcommand{\isa}{\ensuremath{\; {:}{:}{=} \;}}
\newcommand{\ora}{\ensuremath{\;\mid\;}}
\newcommand{\IF}{\ensuremath{\mathtt{ if \;}}}
\newcommand{\THEN}{\ensuremath{\mathtt{\; then \;}}}
\newcommand{\ELSE}{\ensuremath{\mathtt{\; else \;}}}
\newcommand{\TRUE}{\ensuremath{\mathtt{ true \;}}}
\newcommand{\FALSE}{\ensuremath{\mathtt{\; false \;}}}
\newcommand{\BOOL}{\ensuremath{\mathtt{\; Bool \;}}}

\title{EECS 762 - Project 3 Solution}
\author{Perry Alexander \\
  The University of Kansas EECS Department\\
  \texttt{alex@@ittc.ku.edu}}

\begin{document}

\maketitle

\section{Introduction}

The objective of Project 3 is to write an interpreter for simply typed
lambda calculus with subtyping and records ($\lambda_{<:}$)
expressions as defined in \emph{Types and Programming
Languages}~\cite{Pie02a}, Chapters 15 and 16.  In addition, you were
to include booleans and the $\IF$ special form.  The definition of the
abstract syntax provides the following three forms for $\lambda_{<:}$
terms, values and types in:

\begin{eqnarray*}
  t & \isa & x \ora \lambda x:T.t \ora t\; t \ora \\
    &      & t.l \ora \{l_i=t_i^{i\in 1..n}\} \\
  v & \isa & \lambda x:T.t \ora \TRUE \ora \FALSE \ora \{l_i=v_i^{i\in 1..n}\}\\
  T & \isa & \BOOL \ora T \rightarrow T \ora \{l_i:T_i^{i\in 1..n}\} \ora Top
\end{eqnarray*}

The definition for call-by-value evaluation provides the following
evaluation rules that will define the evaluation function:

\[\vcenter{\infer[\textsc{E-App1}]
	{t_1 t_2 \longrightarrow t_1^{'} t_2}
	{t_1\longrightarrow t_1^{'}}}\]

\[\vcenter{\infer[\textsc{E-App2}]
	{t_1 t_2 \longrightarrow t_1 t_2^{'}}
	{t_2\longrightarrow t_2^{'}}}\]

\[\vcenter{\infer[\textsc{E-AppAbs}]
	{(\lambda x:T.t_{12})v_2 \longrightarrow [x\rightarrow v_2]t_{12}}
	{}}\]

\[\vcenter{\infer[\textsc{E-IfTrue}]
	{t_2}
	{\IF \TRUE \THEN t_2 \ELSE t_3}}\]

\[\vcenter{\infer[\textsc{E-IfFalse}]
	{t_3}
	{\IF \FALSE \THEN t_2 \ELSE t_3}}\]

\[\vcenter{\infer[\textsc{E-If}]
  {\IF t_1 \THEN t_2 \ELSE t_3 \rightarrow \IF t_1^{'} \THEN t_2 \ELSE t_3}
  {t\rightarrow t_1^{'}}}\]

\[\vcenter{\infer[\textsc{E-ProjRcd}]
  {\{l_i=v_i^{i\in 1..n}\}.l_j \longrightarrow v_j}
  {}}\]

\[\vcenter{\infer[\textsc{E-Proj}]
  {t_1.l \longrightarrow t_1^{'}.l}
  {t_1 \longrightarrow t_1^{'}}}\]

Finally, the definition provides the following typing rules that will
define the type inference function:

\[\vcenter{\infer[\textsc{T-Var}]{\Gamma\vdash x:T}{x:T\in\Gamma}}\]

\[\vcenter{\infer[\textsc{T-Abs}]
  {\Gamma\vdash\lambda x:T_1.t_2 : T_1\rightarrow T_2}
  {\Gamma,x:T_1\vdash t_2 : T_2}}
\]

\[\vcenter{\infer[\textsc{T-App}]
  {\Gamma\vdash t_1\; t_2 : T_{12}}
  {\Gamma\vdash t_1 : T_{11}\rightarrow T_{12} & \Gamma\vdash t_2:T_{11}}}
\]

\[\vcenter{\infer[\textsc{T-If}]
  {\Gamma\vdash \IF t_1 \THEN t_2 \ELSE t_3 : T}
  {\Gamma\vdash t_1 : \BOOL & \Gamma\vdash t_2 : T & \Gamma\vdash t_3 : T}}
\]

\[\vcenter{\infer{\TRUE:\BOOL}{}}\]

\[\vcenter{\infer{\FALSE:\BOOL}{}}\]

Our objective is to: (i) define a data structure for representing
$\lambda_{<:}$ terms embodying the abstract syntax; (ii) a type
derivation function for $\lambda_{<:}$ terms embodying the type
rules; and (iii) an evaluation function for $\lambda_{<:}$
terms embodying the evaluation rules.

\section{SubtypedLambdaMonad Module}

\begin{code}
  module SubtypedLambdaMonad ( Term(..) )
      where
\end{code}

The |SubtypedLambdaMonad| module provides the basic definitions for
manipulating simply typed lambda calculus expressions with subtyping
and records added.  As indicated by the module name, a monad is used
to implement the evaluation and type checking processes.  The data
type representing terms and types is first defined, followed by the
type checking function, subtyping function, and the evaluation
function.

\subsection{Data Types}

An abstract type |Ty|, is defined to represent the two possible
types in $\lambda_{<:}$.  A constant represents the Boolean
type while a pair of types represents the $\rightarrow$ type former
application.

%  data Retval a = 
%      Value a | Error String
%      deriving (Eq,Show)
%	       
%  instance Monad Retval where
%      Error s >>= k = Error s
%      Value a >>= k = k a 
%      return = Value
%      fail = Error

\begin{code}
  import Control.Monad.Error
  import Data.List

  type Retval a = ErrorT String IO a

  data Ty =
      TyBool |
      TyTop |
      TyArr Ty Ty |
      TyTpl [Ty] |
      TyRec [(String,Ty)]
      deriving (Eq,Show)

  dom :: Ty -> Retval Ty
  dom t =
      case t of
	     TyArr d _ -> return d
	     TyBool -> fail "Type Error - Cannot find domain of a boolean type"
             TyRec _ -> fail "Type Error - Cannot find domain of a record type "
             TyTpl _ -> fail "Type Error - Cannot find the domain of a tuple tye"
             TyTop -> fail "Type Error - Cannot find the domain of top"

  ran :: Ty -> Retval Ty
  ran t =
      case t of
	     TyArr _ r -> return r
	     TyBool -> fail "Type Error - Cannot find range of a boolean type"
             TyRec _ -> fail "Type Error - Cannot find range of a record type"
             TyTpl _ -> fail "Type Error - Cannot find the range of a tuple tye"
             TyTop -> fail "Type Error - Cannot find range of top"

  isArr :: Ty -> Bool
  isArr (TyArr _ _) = True
  isArr _ = False

  isRec :: Ty -> Bool
  isRec (TyRec _) = True
  isRec _ = False

  isTpl :: Ty -> Bool
  isTpl (TyTpl _) = True
  isTpl _ = False
\end{code}

A constructed type, |Term|, is defined to represent the abstract
syntax for $\lambda_{<:}$ terms.  Note that this definition is
identical to that used in $\lambda_{<:}$ except |Lambda|
includes a type annotation, the |If| construct is defined, and boolean
costants have been added.  The implementation of the |Term| type is a
Maybe type that either represents a legal term or a form that cannot
be evaluated.

\begin{code}
  data Term = 
      TmTrue | TmFalse |
      If Term Term Term |
      Var Int |
      Lambda Ty Term |
      AppV Term Term |
      AppN Term Term | 
      Rec [(String,Term)] |
      Tpl [Term] |
      ProjRcd Term String |
      ProjTpl Term Int
      deriving (Eq,Show)
\end{code}      

The |value| function defines a predicate that specifies $\lambda_{<:}$
terms that are values.  By definition, |TmTrue|, |TmFalse|, |Lambda|
forms, |Rec| and |Tpl| are constructors for values.

\begin{code}
  value :: Term -> Bool
  value TmTrue = True
  value TmFalse = True
  value (Lambda _ _) = True
  value (Rec _) = True
  value (Tpl _) = True
  value _ = False
\end{code}

The |TmTrue| and |TmFalse| values represent the Boolean
values true and false respectively.  The |If| constructor
defines a classical if-expression.  The |Var| construtor
identifies the index for a variable and corresponds with the $x$ form
in the abstract syntax.  The |Lambda| constructor defines an
abstraction by specifying a term and corresponds with the $\lambda
x:T.t$ form in the abstract syntax.  The |AppV| cosntructor
defines the application of one term to another using call-by-value
semantics.  Similarly, the |AppN| constructor defines the
appliation of one term to another using call-by-name semantics.  Both
forms correspond with the |t t| form in the abstract syntax.

The original $\lambda$ language implemented untyped lambda
calculus.  Thus, the maintenance of context information was
unnecessary.  For $\lambda_{<:}$, the types of variables must
be maintained as a part of context for type checking.  The
|Ctx| type is used to store a list of variable bindings that
will be used to maintain the types associated with variables in
context.

\begin{code}
  type Ctx = [Ty]
\end{code}

To mainipulate the context, the following functions are defined:

\begin{code}
  addBinding :: Ctx -> Ty -> Ctx
  addBinding ctx t = t:ctx

  getTypeFromContext :: Ctx -> Int -> Retval Ty
  getTypeFromContext c v = if ((length c) >= v) 
			   then return (c!!v)
			   else fail "Type Error - Variable out of scope"

  findField :: Ctx -> String -> Ty -> Retval Ty
  findField ctx s1 (TyRec l) = findField2 ctx s1 l

  findField2 :: Ctx -> String -> [(String,Ty)] -> Retval Ty
  findField2 _ _ [] = fail "Type Error - Field name not found"
  findField2 ctx s1 ((s2,t):l) = if s1 == s2
				 then return t
				 else findField2 ctx s1 l

  findProj :: Ctx -> Int -> Ty -> Retval Ty
  findProj _ i (TyTpl tl) = if i>=0 && i<(length tl)
		    then return (tl !! i)
		    else fail "Type Error - Tuple index out of range"

\end{code} 

\subsection{Type Derivation}

Type derivation is achieved using the |typeof| function.  Each
case directly corresponds to one of the typing rules defined for
$\lambda_{<:}$.

\begin{code}
  typeof :: Ctx -> Term -> Retval Ty
  typeof ctx TmTrue = return TyBool
  typeof ctx TmFalse = return TyBool
  typeof ctx (Rec l) = do tl <- mapM (\(_,x) -> (typeof ctx x)) l;
			  return (TyRec (zip (map (\(x,_) -> x) l) tl))
  typeof ctx (Tpl l) = do tl <- mapM (\x -> (typeof ctx x)) l;
			  return (TyTpl tl)
  typeof ctx (ProjRcd t s) =
      do rectype <- typeof ctx t;
				 if (isRec rectype)
						then findField ctx s rectype
						else fail "Type Error - Cannot project a non-record type."
  typeof ctx (ProjTpl t i) =
      do tpltype <- typeof ctx t;
				 if (isTpl tpltype)
						then findProj ctx i tpltype
						else fail "Type Error - Cannot project a non-tuple type."
  typeof ctx (If t1 t2 t3) = 
      do iftype <- typeof ctx t1;
         if iftype == TyBool
            then do tt1 <- (typeof ctx t2);
										tt2 <- (typeof ctx t3);
										if tt1 == tt2
											 then return tt2
											 else fail "Type Error - If branches of different types"
            else fail "Type Error - If conditional not boolean"
  typeof ctx (Var x) = (getTypeFromContext ctx x)
  typeof ctx (AppV t1 t2) = 
      do tt1 <- typeof ctx t1; 
				 tt2 <- typeof ctx t2;
				 dtt1 <- dom tt1;
         if isArr tt1 then 
            if (subtype tt2 dtt1) then return tt2
               else fail "Type Error - Argument type is not a subtype of domain"
            else fail "Type Error - Term is not an abstraction"           
  typeof ctx (AppN t1 t2) = 
      do tt1 <- typeof ctx t1; 
	 tt2 <- typeof ctx t2;
	 dtt1 <- dom tt1;
         if isArr tt1 then 
            if (subtype tt2 dtt1) then return tt2
               else fail "Type Error - Argument type is not a subtype of domain"
            else fail "Type Error - Term is not an abstraction"           
  typeof ctx (Lambda ty t) =
      do tt <- typeof (addBinding ctx ty) t;
	       return (TyArr ty tt)
\end{code}

The |subtype| function defines when one type is a subtype of another.
|TyBool| is only a subtype of itself.  A function type, |TyArr ty11
ty12| is a subtype of |TyArr ty21 ty22| if |subtype ty21 ty11| and
|subtype ty12 ty22|.  A record, |TyRec| is a subtype of another if
it's list of label, type pairs intersects with the other's label, type
pairs.  |TyTop| is by definition a supertype of everything while all
other subtyping attempts are illegal.

\begin{code}
  subtype :: Ty -> Ty -> Bool
  subtype TyBool TyBool = True
  subtype (TyArr ty11 ty12) (TyArr ty21 ty22) = 
      subtype ty21 ty11 && subtype ty12 ty22
  subtype (TyRec l1) (TyRec l2) = (intersect l1 l2) == l1
  subtype (TyTpl l1) (TyTpl l2) = (isPrefixOf l1 l2)
  subtype _ TyTop = True
  subtype _ _ = False
\end{code}

\subsection{Shifting and Substituting}

Our implementation uses de Brujin indices as a basis for
representation and evaluation.  Thus, definitions of |shift|
and |subst| are required to implement elements of the evalation
function.  These functions are largely the same as those used in
Project 1 except they must deal with type information.

The |shift| definition provides a case for shifting over each
valid $\lambda$ form as defined in the |Term| data type.  This
definition follows directly from the standard definition of shift from
TPL Chapter 8:

\begin{code}
  shift :: Term -> Int -> Int -> Term
  shift TmTrue c d = TmTrue
  shift TmFalse c d = TmFalse
  shift (Rec l) c d = 
      (Rec (map (\ (str,t) -> (str,(shift t c d))) l))
  shift (Tpl l) c d = 
      (Tpl (map (\t -> (shift t c d)) l))
  shift (ProjRcd r s) c d = (ProjRcd (shift r c d) s)
  shift (ProjTpl t i) c d = (ProjTpl (shift t c d) i)
  shift (Var x) c d = if x < c then (Var x) else (Var (x+d))
  shift (Lambda ty t) c d = (Lambda ty (shift t (c+1) d))
  shift (AppV t1 t2) c d = (AppV (shift t1 c d) (shift t2 c d))
  shift (AppN t1 t2) c d = (AppN (shift t1 c d) (shift t2 c d))
\end{code}

|Lambda|, |AppV|, and |AppN| terms are shifted by
shifting their identified terms.  Shifting a |Var| term
requires application of the definition from Chapter 8.  Specifically,
the index for a variable is shifted by |d| if its index is
greater than |c|.

The |subst| definition again provides a case for substitution
over each value $\lambda$ as defined in the |Term| data
type.  This definition follows directly from teh standard definition
of substitution from TPL Chapter 8:

\begin{code}
  subst :: Int -> Term -> Term -> Term
  subst j s (Var x) = if x==j then s else (Var x)
  subst j s (Lambda ty t) = (Lambda ty (subst (j+1) (shift s 0 1) t))
  subst j s (AppV t1 t2) = (AppV (subst j s t1) (subst j s t2))
  subst j s (AppN t1 t2) = (AppN (subst j s t1) (subst j s t2))
  subst _ _ TmTrue = TmTrue
  subst _ _ TmFalse = TmFalse
  subst j s (Rec l) =
      (Rec (map (\ (str,t) -> (str,(subst j s t))) l))
  subst j s (Tpl l) = 
      (Tpl (map (\t -> (subst j s t)) l))
  subst j s (ProjRcd r str) = 
      (ProjRcd (subst j s r) str) 
  subst j s (ProjTpl t i) =
      (ProjTpl (subst j s t) i)
\end{code}

\subsection{Call By Value Evaluation Function}

The |eval| function provides a standard definition of
call-by-value evaluation following from TPL Chapter 5.  The function
definition is split up into cases corresponding to the evaluation
rules.  Note that only |AppN| and |AppV| forms can be
evaluated.  |Lambda| forms are values and |Var| forms
are not closed.  Although passing a context is allowed in this
function, in this project we are concerned only with close terms.
Thus, the context can be safely ignored in all evaluation cases.

The form of the evaluation function is a mapping from |Ctx| and
|Term| to another |Term|:

\begin{code}
  expr :: Term -> Term
  expr (Lambda ty t) = t

  proj :: [(String,Term)] -> String -> Retval Term
  proj [] _ = fail "Evaluation Error - Field not defined"
  proj ((s1,v):l) s2 = if s1==s2
			then return v
			else (proj l s2)   
\end{code}

\begin{code}
  eval :: Term -> Retval Term

  eval TmTrue = return TmTrue
  eval TmFalse = return TmFalse
  eval (Rec x) = return (Rec x)
  eval (Tpl x) = return (Tpl x)

  eval (AppV t1 t2) =
      do nt1 <- eval t1;
	 nt2 <- eval t2;
	 eval (shift (subst 0 (shift nt2 0 1) (expr nt1)) 0 (-1))
		   
  eval (AppN t1 t2) =
      do nt1 <- eval t1;
	 eval (shift (subst 0 (shift t2 0 1) (expr nt1)) 0 (-1))

  eval (Lambda ty1 t1) = return (Lambda ty1 t1)
			      
  eval (Var t1) = fail "Evaluation Error - Cannot evaluate free variable"

  eval (If TmTrue t2 t3) = 
      do tr <- eval t2;
	 return tr
  eval (If TmFalse t2 t3) = 
      do fl <- eval t3;
	 return fl
  eval (If t1 t2 t3) = 
      do cond <- (eval t1);
	 eval (If cond t2 t3)
  eval (ProjRcd (Rec r) l) = (proj r l)
  eval (ProjTpl (Tpl t) i) = return (t !! i)
\end{code}

\section{Type Checking and Evaluation}

The objective of type checking is to statically predict the runtime
behavior of a code element with respect to the kinds of values
produced and expected.  Specifically, does a program element always
product the kind of value espected?  The \texttt{typeof} function
provides a capability for generating the type associated with a
$\lambda_{<:}$ term.  If such a type exists, then the term is
well-typed and should be executed.

To combine type checking and evaluation is a simple matter of: (i)
determining the type of a term; and (ii) executing the term if the
type exists.  This process can be specified using the following
template:

\begin{code}
  interpTemplate :: (Term -> Retval Ty) ->
		    (Term -> Retval Term) ->
		    Term ->
		    Retval Term
  interpTemplate typeof eval term =
      do termtype <- (typeof term);
	 term' <- (eval term);
	 return term'

  evalTemplateStar :: (Term -> Retval Term) -> Term -> Retval Term
  evalTemplateStar eval term =
      do term' <- (eval term);
	 if (value term')
	    then return term'
	    else (evalTemplateStar eval) term'

\end{code}

\begin{code}
  interpret :: Term -> Retval Term
  interpret = interpTemplate (typeof []) eval

  interpret2 :: Term -> Retval Term
  interpret2 term = do x <- interpret term;
		       return x

\end{code}

\section{Testing and Evaluation}

To test the interpreter, some functions from the book are provided
here.  They include the identity combinator, Church Boolean funtions,
some Church Numbers and the successor function defined for Church
numbers.

Note that with the introduction of types, all parameters must have
associated type annotations.  Although significant static checking
results, the flexibility of these functions is diminished.
Specifically, for the \texttt{ident} combinator a new combinator must
be written for each type.  No polymorphism exists and the type system
is strict, so there is no way to reuse the \texttt{ident} combinator
definition.  Such strictness is common in older programming languages,
but new polymorphism implementations render the approach obscolete.

\subsection{Identity Combinator}

Redefine the |ident| combinator to operate over |Top| to take
advantage of subtyping.  Evaluating the |(App indent t)| combinator on
term defined in $\lambda_{<:}$ should result in |t| regardless of its type:

\begin{code}
  ident :: Term
  ident = (Lambda TyTop (Var 0))
--  testIdent :: [Retval Term]
  testIdent =
      mapM genTestOutput
	       [ident,
		(Rec [("1",TmTrue),("2",TmFalse),("3",ident)]),
		(ProjRcd (Rec [("1",TmTrue),("2",TmFalse),("3",ident)]) "1")]
  
  --testIdent2 = map interpret [ident,(Var 0)]

-- Use runErrorT to see the results for some still unknown reason...
--  testIdent2 :: Retval Term
  testIdent2 = do x <- interpret ident;
		  y <- typeof [] x;
--		  liftIO (putStr (show y) ++ "\n");
		  liftIO (putStr ((show x) ++ " :: " ++ (show y) ++ "\n"))
--		  return x

  genTestOutput t = do x <- interpret t;
		       y <- typeof [] x;
		       liftIO (putStr ((show x) ++ " :: " ++ (show y) ++ "\n"))


--  testRecord ::[Retval Term]
  testRecord =
      let r = (Rec [("1",TmTrue),("2",TmFalse),("3",ident)]) in
	  mapM genTestOutput
		   [(ProjRcd r "1"),
		    (ProjRcd r "3"),
		    (ProjRcd r "4"),
		    (AppV (ProjRcd r "3") r)]

  testRecSubtype :: [Retval Term]
  testRecSubtype =
      let l = (Lambda (TyRec [("1",TyBool),("2",TyBool),("3",TyBool)])
	       (ProjRcd (Var 0) "2")) in
	  map interpret
		  [(AppV l (Rec [("1",TmTrue),("2",TmFalse),("3",TmTrue)])),
		   (AppV l (Rec [("1",TmTrue),("2",TmFalse)])),
		   (AppV l (Rec [("2",TmFalse),("3",TmTrue)])),
		   (AppV l (Rec [("1",TmTrue),("4",TmFalse),("3",TmTrue)]))]
  testTuple :: [Retval Term]
  testTuple =
      let r = (Tpl [TmTrue,TmFalse,ident]) in
	  map interpret
	      [(ProjTpl r 0),
	       (ProjTpl r 2),
	       (ProjTpl r 3),
	       (AppV (ProjTpl r 2) r)]

  testTupleSubtype :: [Retval Term]
  testTupleSubtype =
      let l = (Lambda (TyTpl [TyBool,TyBool,TyBool])
	       (ProjTpl (Var 0) 1)) in
	  map interpret
		  [(AppV l (Tpl [TmTrue,TmFalse,TmTrue])),
		   (AppV l (Tpl [TmTrue,TmFalse])),
		   (AppV l (Tpl [TmFalse,TmTrue])),
		   (AppV l (Tpl [TmTrue,ident,TmTrue]))]

\end{code}

\subsection{Church Boolean Definitions}

The boolean combinators must have typed parameters.  Real booleans are
chosen for simplicity.  Church Booleans don't make a great deal of
sense in a strongly typed language like this.

\begin{code}
  tru :: Term
  tru = (Lambda TyTop (Lambda TyTop (Var 1)))
  fls :: Term
  fls = (Lambda TyTop (Lambda TyTop (Var 0)))

  testTru :: Retval Term
  testTru = interpret (AppV (AppV tru ident) fls)

  testFls :: Retval Term
  testFls = interpret (AppV (AppV fls fls) ident)
\end{code}

\subsection{Church Number Definitions}

The number combinators must have typed parameters.  Real booleans are
chosen for simplicity.  In a word, these definitions are not appropriate
anymore, but they are retained for testing.

\begin{code}
  c0 :: Term
  c0 = (Lambda TyBool (Lambda TyBool (Var 0)))

  c1 :: Term
  c1 = (Lambda TyBool (Lambda TyBool (AppV (Var 1) (Var 0))))

  c2 :: Term
  c2 = (Lambda TyBool (Lambda TyBool (AppV (Var 1) (AppV (Var 1) (Var 0)))))

  scc :: Term
  scc = (Lambda TyBool
         (Lambda TyBool
          (Lambda TyBool (AppV (Var 1)
                          (AppV (Var 2)
                           (AppV (Var 1) (Var 0)))))))
\end{code}

\subsection{Omega}

With the introduction of types, the omega combinator must type its
parameter.  Boolean is selected here for the sake of simplicity, but
other types could be used.

\begin{code}
  omega :: Term
  omega = (Lambda TyTop (AppV (Var 0) (Var 0)))
\end{code}

\section{Notes}

The evaluation function is built to handle both call-by-value and
call-by-name function application.  This may end up causing problems
if the arguments to record and tuple references are treated as types
in the language rather than Haskell string and integer types.

Are record and tuple projections too aggressive for call-by-name
application?  They are not true functions in the langauge due to the
types of their reference values.

\bibliography{prog-langs}

\end{document}
