\documentclass[10pt]{article}

\usepackage{haskell}
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

\title{EECS 762 - Project 2 Solution}
\author{Perry Alexander \\
  The University of Kansas EECS Department\\
  \texttt{alex@ittc.ku.edu}}

\begin{document}

\maketitle

\section{Introduction}

The objective of Project 2 is to write an interpreter for simply
typed lambda calculus ($\lambda_\rightarrow$) expressions as defined
in \emph{Types and Programming Languages}~\cite{Pie02a}, Chapter 8,
Figure 8-1 and Chapter 9, Figure 9-1.  The definition of the abstract
syntax provides the following three forms for $\lambda_\rightarrow$
terms, values and types in:

\begin{eqnarray*}
  t & \isa & x \ora \lambda x:T.t \ora t\; t \\
  v & \isa & \lambda x:T.t \\
  T & \isa & \BOOL \ora T \rightarrow T
\end{eqnarray*}

The definition for call-by-value evaluation provides the following
six evaluation rules that will define the evaluation function:

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
  {t\rightarrow t_1^{'}}}
\]

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
$\lambda_\rightarrow$ terms embodying the abstract syntax; (ii) a type
derivation function for $\lambda_\rightarrow$ terms embodying the type
rules; and (iii) an evaluation function for $\lambda_\rightarrow$
terms embodying the evaluation rules.

\section{TypedLambda Module}

\begin{code}
  module TypedLambda
      where
      
  import LangUtils
  import TypedLambdaAST
  import TypedLambdaTypes
--  import TypedLamdbaEval
\end{code}

The \texttt{TypedLambda} module provides the basic definitions for
manipulating simply typed lambda calculus expressions.  The data type
representing terms and types is first defined, followed by the type
checking function and the evaluation function.

The \texttt{TmTrue} and \texttt{TmFalse} values represent the Boolean
values true and false respectively.  The \texttt{If} constructor
defines a classical if-expression.  The \texttt{Var} constructor
identifies the index for a variable and corresponds with the $x$ form
in the abstract syntax.  The \texttt{TmLambda} constructor defines an
abstraction by specifying a term and corresponds with the $\lambda
x:T.t$ form in the abstract syntax.  The \texttt{App} cosntructor
defines the application of one term to anothre and corresponds with
the \texttt{t t} form in the abstract syntax.  The \texttt{CantEval}
constructor identifies a form that cannot be evaluated.  It's argument
is a \texttt{String} representing the form or an error message.

The \texttt{CantEval} constructor is necessary due to the way
the evaluation function operates.  Unlike the ML implementation,
exceptions are not used to break out of an iterative evaluation
process.  Instead, the evaluation will be done recursively and the
\texttt{CantEval} constructor used to identify error situations.

The original $\lambda$ language implemented untyped lambda calculus.
Thus, the maintenance of context information was unnecessary.  For
$\lambda_\rightarrow$, the types of variables must be maintained as a
part of context for type checking.  The \texttt{Ctx} type is used to
store a list of variable bindings that will be used to maintain the
types associated with variables in context.

\begin{spec}
  type Ctx = [Ty]
\end{spec}

To mainipulate the context, the following functions are defined:

\begin{spec}
  addBinding :: Ctx -> Ty -> Ctx
  addBinding cx t = t:cx

  getTypeFromContext :: Ctx -> Int -> Ty
  getTypeFromContext c v = c!!v
\end{spec} 

\subsection{Type Derivation}

Type derivation is achieved using the \texttt{typeof} function.  Each
case directly corresponds to one of the typing rules defined for
$\lambda_\rightarrow$.

\begin{spec}
  typeof :: Ctx -> Term -> Ty
  typeof ctx TmTrue = TyBool
  typeof ctx TmFalse = TyBool
  typeof ctx (If t1 t2 t3) =
      if (typeof ctx t1) == TyBool
         then let tt1 = typeof ctx t2; tt2 = typeof ctx t3 in
                  if tt1 == tt2 then tt2
                     else TyError "If branches of different types"
         else TyError "If conditional not boolean"
  typeof ctx (Var x) = getTypeFromContext ctx x
  typeof ctx (App t1 t2) = 
      let tt1 = typeof ctx t1; tt2 = typeof ctx t2 in
         if isArr tt1 then 
            if tt2 == dom(tt1) then tt2
               else TyError "Argument type does not match domain"
            else TyError "Term is not an abstraction"           
  typeof ctx (TmLambda ty t) =
      let tt = typeof (addBinding ctx ty) t in
         (TyAbs ty tt)
\end{spec}

\subsection{Shifting and Substituting}

Our implementation uses de Brujin indices as a basis for
representation and evaluation.  Thus, definitions of \texttt{shift}
and \texttt{subst} are required to implement elements of the evalation
function.  These functions are largely the same as those used in
Project 1 except they must deal with type information.

The \texttt{shift} definition provides a case for shifting over each
valid $\lambda$ form as defined in the \texttt{Term} data type.  This
definition follows directly from the standard definition of shift from
TPL Chapter 8:

\begin{spec}
  shift :: Term -> Int -> Int -> Term
  shift TmTrue c d = TmTrue
  shift TmFalse c d = TmFalse
  shift (Var x) c d = if x < c then (Var x) else (Var (x+d))
  shift (TmLambda ty t) c d = (TmLambda ty (shift t (c+1) d))
  shift (App t1 t2) c d = (App (shift t1 c d) (shift t2 c d))
\end{spec}

Both \texttt{TmLambda} and \texttt{App} terms are shifted by shifting
their identified terms.  Shifting a \texttt{Var} term requires
application of the definition from Chapter 8.  Specifically, the index
for a variable is shifted by \texttt{d} if its index is greater than
\texttt{c}.

The \texttt{subst} definition again provides a case for substitution
over each value $\lambda$ for as defined in the \texttt{Term} data
type.  This definition follows directly from teh standard definition
of substitution from TPL Chapter 8:

\begin{spec}
  subst :: Int -> Term -> Term -> Term
  subst j s (Var x) = if x==j then s else (Var x)
  subst j s (TmLambda ty t) = (TmLambda ty (subst (j+1) (shift s 0 1) t))
  subst j s (App t1 t2) = (App (subst j s t1) (subst j s t2))
\end{spec}

\subsection{Call By Value Evaluation Function}

The \texttt{evalValue} function provides a standard definition of
call-by-value evaluation following from TPL Chapter 5.  The function
definition is split up into cases corresponding to the evaluation
rules.  Note that only \texttt{App} forms can be evaluated.
\texttt{TmLambda} forms are values and \texttt{Var} forms are not
closed.  Although passing a context is allowed in this function, in
this project we are concerned only with close terms.  Thus, the
context can be safely ignored in all evaluation cases.

The form of the evaluation function is a mapping from \texttt{Ctx} and
\texttt{Term} to another \texttt{Term}

\begin{spec}
  evalValue :: Ctx -> Term -> Term
\end{spec}

The first case to handle is \textsc{E-AppAbs} that evaluates an
application.  Both terms in the abstraction must be values.  For
$\lambda$, only abstractions are values, thus one case is sufficient
for evaluation.  $\lambda_\rightarrow$ introduces Boolean values that
must be accounted for.  The definition of \texttt{shift} is modified
to operate over Boolean terms and the evaluat call remains virtually
the same in the remaining two cases.  The decision to modify
\texttt{shift} rather than handle Booleans as special cases here also
handles \texttt{evalLazy} later.  The definition of evaluation is
taken directly from TPL Chapters 7 and 8:

\begin{spec}
  evalValue _ (App (TmLambda ty12 t12) (TmLambda ty2 v2)) =
        let nt = shift (subst 0 (shift (TmLambda ty2 v2) 0 1) t12) 0 (-1) in
            if (isApp nt) then evalValue [] nt else nt
  evalValue _ (App (TmLambda ty12 t12) TmTrue) =
        let nt = shift (subst 0 (shift TmTrue 0 1) t12) 0 (-1) in
            if (isApp nt) then evalValue [] nt else nt
  evalValue _ (App (TmLambda ty12 t12) TmFalse) =
        let nt = shift (subst 0 (shift TmFalse 0 1) t12) 0 (-1) in
            if (isApp nt) then evalValue [] nt else nt
\end{spec}

The evaluation rule \textsc{E-App1} is used when the first term of the
application is not a value.  It is handled by two subcases
corresponding with \texttt{App} and \texttt{Var} forms.  In the first
subcase, the first term is an application and is evaluated before
evaluating the outer application.  In the second subcase, the first
term is a variable and is again evaluation before evaluating the outer
application.

\begin{spec}
  evalValue c (App (App t11 t12) t) =
      let nt = evalValue c (App t11 t12) in evalValue c (App nt t)
  evalValue c (App (Var x) t) = 
      let nt = evalValue c (Var x) in evalValue c (App nt t)
\end{spec}

The second subcase will always lead to an error in this
implementation, but future implementations will not have this
restriction.  Thus, it is defined strictly rather than taking a
shortcut to the error generation.

The evaluation rule \textsc{E-App2} is used when the first term of the
application is a value, but the second is not.  It is again handled by
two subcases that correspond to the same cases as \textsc{E-App1},
except the second term is being evaluated:

\begin{spec}
  evalValue c (App (TmLambda ty12 t12) (App t21 t22)) =
      let nt = evalValue c (App t21 t22) in evalValue c (App t21 nt)
  evalValue c (App (TmLambda ty12 t12) (Var x)) = 
      evalValue c (App (TmLambda ty12 t12) (evalValue c (Var x)))
\end{spec}

The previous cases of \texttt{evalValue} account for the successful
application of evaluation rules.  However, error cases and the case
for evaluating values remain to be defined.

In $\lambda_\rightarrow$, the boolean constants $\TRUE$ and $\FALSE$ in
addition to all lambda expressions are treated as values.  By
definition, a value cannot be further evaluated.  Thus, evaluating a
\texttt{TmLambda} form, $\TRUE$, or $\FALSE$ results in the value itself:

\begin{spec}
  evalValue _ (TmLambda ty t) = (TmLambda ty t)
  evalValue _ TmTrue = TmTrue
  evalValue _ TmFalse = TmFalse
\end{spec}

Three additional cases define the evaluation of the $\IF$ form.  If
the conditional is a value, then the $\IF$ form reduces to its
appropriate form.  I will follow strictly the evaluation rules from
Chapter 3 Figure 3-1 that do not define the evaluation of the $\IF$
form in the strictest possible manner.

\begin{spec}
  evalValue _ (If TmTrue t2 _) = t2
  evalValue _ (If TmFalse _ t3) = t3
  evalValue c (If (App t11 t12) t2 t3) = 
    (If (evalValue c (App t11 t12)) t2 t3)
\end{spec}

With the evaluation of values defined, the only remaining issue is the
error case caused when a free variable is evaluated.  Three cases of
\texttt{evalValue} handle the generation of the error and propagation
back through the recursive invocation of \texttt{evalValue}:

\begin{spec}
  evalValue _ (Var x) = (CantEval (show (Var x)))
  evalValue _ (App (CantEval x) t) = (CantEval x)
  evalValue _ (App t (CantEval x)) = (CantEval x)
\end{spec}

The first case generates the error when a free variable is evaluated.
It converts the variable into a string and uses the \texttt{CantEval}
constructor to generate an error.  The error is propagated back
through the recursion using the two following cases.  Together they
state that the result of evaluating an \texttt{App} is
\texttt{CantEval} if either of its terms are \texttt{CantEval}.

\subsection{Call By Name Evaluation Function}

Call-by-name evaluation uses the same definition of terms and values
as call-by-value.  However, two different evaluation rules define the
evaluation relation:

\[\vcenter{\infer[\textsc{E-App1}]
	{t_1 t_2 \longrightarrow t_1^{'} t_2}
	{t_1\longrightarrow t_1^{'}}}\]

\[\vcenter{\infer[\textsc{E-AppAbs}]
	{(\lambda x.t_{12})t_2 \longrightarrow [x\rightarrow t_2]t_{12}}
	{}}\]

The \textsc{E-App1} rule is identical to the rule for call-by-value.
However, the \textsc{E-AppAbs} rule does not require the argument to
the \texttt{TmLambda} to be a value and the old \textsc{E-App2}
evaluation rule is no longer needed.

Boolean evaluation rules remain the same as the call-by-value version
to assure determinacy in the evaluation function.  Note that other
interpretations are acceptable.

\[\vcenter{\infer[\textsc{E-IfTrue}]
	{t_2}
	{\IF \TRUE \THEN t_2 \ELSE t_3}}\]

\[\vcenter{\infer[\textsc{E-IfFalse}]
	{t_3}
	{\IF \FALSE \THEN t_2 \ELSE t_3}}\]

\[\vcenter{\infer[\textsc{E-If}]
  {\IF t_1 \THEN t_2 \ELSE t_3 \rightarrow \IF t_1^{'} \THEN t_2 \ELSE t_3}
  {t\rightarrow t_1^{'}}}
\]

The \texttt{evalLazy} function provides a standard definition of
call-by-name evaluation following from TPL problem solutions.  The
function definition is split up into cases corresponding to the
evaluation rules identically to \texttt{evalValue}.  Again, only
\texttt{App} forms can be evaluated, \texttt{TmLambda} forms are values
and \texttt{Var} forms are not closed and thus not considered.

\begin{spec}
  evalLazy :: Ctx -> Term -> Term
\end{spec}

Again, the first case to handle is \textsc{E-AppAbs} that evaluates an
application.  The first term must be a value.  In call-by-name
evaluation, it is not necessary for the second term to be a value.
Thus, the pattern match to assure that the second term is a
\texttt{TmLambda} is removed.  The definition of evaluation is taken
directly from TPL Chapters 7 and 8:

\begin{spec}
  evalLazy _ (App (TmLambda ty12 t12) t2) =
     let nt = shift (subst 0 (shift t2 0 1) t12) 0 (-1) in
         if (isApp nt) then (evalLazy [] nt) else nt
\end{spec}

The second and third cases occur when the first \texttt{App} term is
not a value.  The first case handles when the first term is an
\texttt{App} form and the second when the first term is a \texttt{Var}
form.  These cases are virtually identical to those for call-by-value.

\begin{spec}
  evalLazy c (App (App t11 t12) t) =
      let nt = evalLazy c (App t11 t12) in evalLazy c (App nt t)
  evalLazy c (App (Var x) t) = 
      let nt = evalLazy c (Var x) in evalLazy c (App nt t)
\end{spec}

Both cases for \textsc{E-App2} from the call-by-value implementation
are no longer needed because the \textsc{E-App2} rule is gone.  The
\textsc{E-AppAbs} cases will match when \textsc{E-App2} used to
because the second term in the \texttt{App} need not be a value.

Like call-by-value, \texttt{TmLambda} forms, \texttt{true} and
\texttt{false} are values.  Thus, evaluating them in the call-by-name
scheme is identical to call-by-value.

\begin{spec}
  evalLazy _ (TmLambda ty t) = (TmLambda ty t)
  evalLazy _ TmTrue = TmTrue
  evalLazy _ TmFalse = TmFalse
\end{spec}

Three additional cases define the evaluation of the $\IF$ form that
are identical to thos for call-by-value.  If the conditional is a
value, then the $\IF$ form reduces to its appropriate form.

\begin{spec}
  evalLazy _ (If TmTrue t2 _) = t2
  evalLazy _ (If TmFalse _ t3) = t3
  evalLazy c (If (App t11 t12) t2 t3) = 
    (If (evalLazy c (App t11 t12)) t2 t3)
\end{spec}

Error cases for call-by-name are a subset of error cases for
call-by-value.  Evaluating a free variable still generates an error
and an error occuring during evaluation of the first \texttt{App} term
is propagated back.  However, in call-by-name evaluation the second
term is not evaluated until necessary and the error case from
call-by-value does not occur.

\begin{spec}
  evalLazy _ (Var x) = (CantEval (show (Var x)))
  evalLazy _ (App (CantEval x) t) = (CantEval x)
\end{spec}

\section{Type Checking and Evaluation}

The objective of type checking is to statically predict the runtime
behavior of a code element with respect to the kinds of values
produced and expected.  Specifically, does a program element always
product the kind of value espected?  The \texttt{typeof} function
provides a capability for generating the type associated with a
$\lambda_\rightarrow$ term.  If such a type exists, then the term is
well-typed and should be executed.

To combine type checking and evaluation is a simple matter of: (i)
determining the type of a term; and (ii) executing the term if the
type exists.  This process can be specified using the following
template:

\begin{spec}
  evalTemplate :: (Term -> Ty) -> (Term -> Term) -> Term -> Term
  evalTemplate typeof eval t =
    let tt = (typeof t) in
	if (isTyError tt) then (errorToTerm tt) else (eval t)

  errorToTerm :: Ty -> Term
  errorToTerm (TyError s) = CantEval s
\end{spec}

The \texttt{errorToTerm} function is needed to convert a type checking
error to an evaluation error.  The generated error message is simply
pulled out of the type error structure and returned in a run-time
error structure.

With the template defined, call-by-value and call-by-name evaluation
functions are defined as follows:

\begin{spec}
  evalV :: Term -> Term
  evalV = evalTemplate (typeof []) (evalValue [])

  evalL :: Term -> Term
  evalL = evalTemplate (typeof []) (evalLazy [])
\end{spec}

In both cases, the \texttt{evalTemplate} function is curried with the
type checking and evaluation functions.

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

The identity combinator must have a typed parameter.  For simplicity,
\texttt{Bool} is used here:

\begin{spec}
  ident :: Term
  ident = (TmLambda TyBool (Var 0))
\end{spec}

\subsection{Church Boolean Definitions}

The boolean combinators must have typed parameters.  Real booleans are
chosen for simplicity.  Church Booleans don't make a great deal of
sense in a strongly typed language like this.

\begin{spec}
  tru :: Term
  tru = (TmLambda TyBool (TmLambda TyBool (Var 1)))
  fls :: Term
  fls = (TmLambda TyBool (TmLambda TyBool (Var 0)))
\end{spec}

\subsection{Church Number Definitions}

The number combinators must have typed parameters.  Real booleans are
chosen for simplicity.  In a word, these definitions are not appropriate
anymore, but they are retained for testing.

\begin{spec}
  c0 :: Term
  c0 = (TmLambda TyBool (TmLambda TyBool (Var 0)))

  c1 :: Term
  c1 = (TmLambda TyBool (TmLambda TyBool (App (Var 1) (Var 0))))

  c2 :: Term
  c2 = (TmLambda TyBool (TmLambda TyBool (App (Var 1) (App (Var 1) (Var 0)))))

  scc :: Term
  scc = (TmLambda TyBool
         (TmLambda TyBool
          (TmLambda TyBool (App (Var 1)
                          (App (Var 2)
                           (App (Var 1) (Var 0)))))))
\end{spec}

\subsection{Omega}

With the introduction of types, the omega combinator must type its
parameter.  Boolean is selected here for the sake of simplicity, but
other types could be used.

\begin{spec}
  omega :: Term
  omega = (TmLambda TyBool (App (Var 0) (Var 0)))
\end{spec}

\subsection{Example Tests}

Tests to try on Church Booleans include:

\begin{spec}
  testTru :: Term
  testTru = (App (App tru ident) fls)

  testFls :: Term
  testFls = (App (App fls fls) ident)
\end{spec}

\bibliography{prog-langs}

\end{document}
