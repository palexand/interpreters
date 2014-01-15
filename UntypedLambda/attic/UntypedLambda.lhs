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
definition of the abstract syntax provides the following two forms
for terms and values:

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

\begin{code}
  module UntypedLambda ( Term(..) )
      where
\end{code}

The \texttt{UntypedLambda} module provides the basic definitions for
manipulating lambda calculus expressions.  The data type representing
terms is first defined, followed by the evaluation functions.  The data
type for expressions provides program representations for each of the
three abstract syntax forms defined for $\lambda$.  The evaluation
functions provide both call-by-value and call-by-name evaluation
capabilities.

\subsection{Data Types}

An abstract type, \texttt{Term}, is defined to represent $lambda$
terms for evaluation.  The implementation of the \texttt{Term} type
either represents a legal term or a form that cannot be evaluated.

\begin{code}
  data Term = 
      Var Int |
      Lambda Term |
      App Term Term |
      CantEval String
      deriving (Eq,Show)

  isApp :: Term -> Bool
  isApp t1
	= case t1 of
	    App _ _ -> True
	    Lambda _ -> False
	    Var _ -> False
	    CantEval _ -> False
\end{code}

The \texttt{Var} constructor identifies the index for a variable and
corresponds with the $x$ form in the abstract syntax.  The
\texttt{Lambda} constructor defines an abstraction by specifying a
term and corresponds with the $\lambda x.t$ form in the abstract
syntax.  The \texttt{App} constructor defines the application of one
term to another and corresponds with the \texttt{t t} form in the
abstract syntax.  The \texttt{CantEval} constructor identifies a form
that cannot be evaluated.  It's argument is a \texttt{String}
representing the form or an error message.

The \texttt{CantEval} constructor is necessary due to the need for
evaluation functions to return both terms and error messages.  Unlike
the ML implementation, exceptions are not used to break out of an
iterative evaluation process.  Instead, evaluation will be done
recursively and the \texttt{CantEval} constructor used to identify
error situations.

The evaluation functions defined later will accept a context as one
parameter.  For closed $\lambda$ expressions, the context value is not
necessary.  For future evaluators, the context may be required, thus a
placeholder is provided.  The context type, \texttt{Ctx}, is defined
to be the unit type whose only member is the unit value:

\begin{code}
  type Ctx = ()
\end{code}

Note that you could safely leave out context handling for this project
entirely without penalty.

\subsection{Shifting and Substituting}

Our implementation uses de Brujin indexes as a basis for
representation and evaluation.  Thus, definitions of \texttt{shift}
and \texttt{subst} are required to implement elements of the evaluation
function.

The \texttt{shift} definition provides a case for shifting over each
valid $\lambda$ form as defined in the \texttt{Term} data type.  The
\texttt{CantEval} constructor is not included here because it will
never be evaluated.  This definition follows directly from the
standard definition of shift from TPL Chapter 8:

\begin{code}
  shift :: Term -> Int -> Int -> Term
  shift (Var x) c d = if x < c then (Var x) else (Var (x+d))
  shift (Lambda t) c d = (Lambda (shift t (c+1) d))
  shift (App t1 t2) c d = (App (shift t1 c d) (shift t2 c d))
\end{code}

Both \texttt{Lambda} and \texttt{App} terms are shifted by shifting
their terms.  Shifting a \texttt{Var} term requires application of the
definition from Chapter 8.  Specifically, the index for a variable is
incremented by \texttt{d} if its index is greater than \texttt{c}.

The \texttt{subst} definition again provides a case for substitution
over each value $\lambda$ form other than \texttt{CantEval}as defined
in the \texttt{Term} data type.  This definition follows directly from
the standard definition of substitution from TPL Chapter 8:

\begin{code}
  subst :: Int -> Term -> Term -> Term
  subst j s (Var x) = if x==j then s else (Var x)
  subst j s (Lambda t) = (Lambda (subst (j+1) (shift s 0 1) t))
  subst j s (App t1 t2) = (App (subst j s t1) (subst j s t2))
\end{code}

\subsection{Call By Value Evaluation Function}

The \texttt{evalValue} function provides a standard definition of
call-by-value evaluation following from TPL Chapter 5.  Both
\texttt{evalValue} and the later definition of \texttt{evalLazy}
define $t\longrightarrow^* t'$ rather than $t\longrightarrow t'$.  The
function definition is split up into cases corresponding to the
evaluation rules.  Note that only \texttt{App} forms can be evaluated.
\texttt{Lambda} forms are values and \texttt{Var} forms are not
closed.  Although passing a context is allowed in this function, in
this project we are concerned only with close terms.  Thus, the
context can be safely ignored in all evaluation cases.

The form of the evaluation function is a mapping from \texttt{Ctx} and
\texttt{Term} to another \texttt{Term}

\begin{code}
  evalValue :: Ctx -> Term -> Term
\end{code}

The first case to handle is \textsc{E-AppAbs} that evaluates an
application.  Both terms in the abstraction must be values.  Because
all \texttt{Lambda} forms are values, pattern matching is used to
assure values for this particular case.  The definition of evaluation
is taken directly from TPL Chapters 7 and 8:

\begin{code}
  evalValue _ (App (Lambda t12) (Lambda v2)) =
        let nt = shift (subst 0 (shift (Lambda v2) 0 1) t12) 0 (-1) in
            if (isApp nt) then evalValue () nt else nt
\end{code}

The \texttt{let} form performs single-step evaluation on the
\texttt{App} construct.  The \texttt{if} form following determines if
the resulting expression is a value.  If it is, evaluation terminates
tha the resulting structure is returned.  If it is not, the result is
evaluated recursively.  If the $t\longrightarrow t'$ form is required,
the \texttt{let} should be removed and only one evaluation step
performed.

The evaluation rule \textsc{E-App1} is used when the first term of the
application is not a value.  It is handled by two sub-cases
corresponding with \texttt{App} and \texttt{Var} forms.  In the first
sub-case, the first term is an application and is evaluated before
evaluating the outer application.  In the second sub-case, the first
term is a variable and is again evaluation before evaluating the outer
application.

\begin{code}
  evalValue c (App (App t11 t12) t) =
      let nt = evalValue c (App t11 t12) in evalValue c (App nt t)
  evalValue c (App (Var x) t) = 
      let nt = evalValue c (Var x) in evalValue c (App nt t)
\end{code}

The second sub-case will always lead to an error in this
implementation, but future implementations may not have this
restriction.  Thus, it is defined strictly rather than short
circuiting and generating an error message directly.

The evaluation rule \textsc{E-App2} is used when the first term of the
application is a value, but the second is not.  It is again handled by
two sub-cases that correspond to the same cases as \textsc{E-App1},
except the second term is being evaluated:

\begin{code}
  evalValue c (App (Lambda t12) (App t21 t22)) =
      let nt = evalValue c (App t21 t22) in evalValue c (App t21 nt)
  evalValue c (App (Lambda t12) (Var x)) = 
      evalValue c (App (Lambda t12) (evalValue c (Var x)))
\end{code}

The previous cases of \texttt{evalValue} account for the successful
application of evaluation rules.  However, error cases and the case
for evaluating values remain to be defined.

In $\lambda$, all lambda expressions are treated as values.  By
definition, a value cannot be further evaluated.  Thus, evaluating a
\texttt{Lambda} form results in the \texttt{Lambda} itself:

\begin{code}
  evalValue _ (Lambda t) = (Lambda t)
\end{code}

With the evaluation of values defined, the only remaining issue is the
error case caused when a free variable is evaluated.  Three cases of
\texttt{evalValue} handle the generation of the error and propagation
back through the recursive invocation of \texttt{evalValue}:

\begin{code}
  evalValue _ (Var x) = (CantEval (show (Var x)))
  evalValue _ (App (CantEval x) t) = (CantEval x)
  evalValue _ (App t (CantEval x)) = (CantEval x)
\end{code}

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
  {(\lambda x.t_{12})t_2 \longrightarrow [x\rightarrow t_2]t_{12}}{}}\]

The \textsc{E-App1} rule is identical to the rule for call-by-value.
However, the \textsc{E-AppAbs} rule does not require the argument to
the \texttt{Lambda} to be a value and the old \textsc{E-App2}
evaluation rule is no longer needed.

The \texttt{evalLazy} function provides a standard definition of
call-by-name evaluation following from TPL problem solutions.  Again,
$t\longrightarrow^*t'$ is implemented rather than a single step
evaluation function.  The function definition is split up into cases
corresponding to the evaluation rules identically to
\texttt{evalValue}.  Again, only \texttt{App} forms can be evaluated,
\texttt{Lambda} forms are values and \texttt{Var} forms are not closed
and thus not considered.

\begin{code}
  evalLazy :: Ctx -> Term -> Term
\end{code}

The first case to handle is \textsc{E-AppAbs} that evaluates an
application.  The first term must be a value.  In call-by-name
evaluation, it is not necessary for the second term to be a value.
Thus, the pattern match to assure that the second term is a
\texttt{Lambda} is removed.  The definition of evaluation is taken
directly from TPL Chapters 7 and 8:

\begin{code}
  evalLazy _ (App (Lambda t12) t2) =
     let nt = shift (subst 0 (shift t2 0 1) t12) 0 (-1) in
         if (isApp nt) then (evalLazy () nt) else nt
\end{code}

The second and third cases occur when the first \texttt{App} term is
not a value.  The first case handles when the first term is an
\texttt{App} form and the second when the first term is a \texttt{Var}
form.  These cases are virtually identical to those for call-by-value.

\begin{code}
  evalLazy c (App (App t11 t12) t) =
      let nt = evalLazy c (App t11 t12) in evalLazy c (App nt t)
  evalLazy c (App (Var x) t) = 
      let nt = evalLazy c (Var x) in evalLazy c (App nt t)
\end{code}

Both cases for \textsc{E-App2} from the call-by-value implementation
are no longer needed because the \textsc{E-App2} rule is gone.  The
\textsc{E-AppAbs} cases will match when \textsc{E-App2} used to
because the second term in the \texttt{App} need not be a value.

Like call-by-value, \texttt{Lambda} forms are values.  Thus,
evaluating them in the call-by-name scheme is identical to
call-by-value. 

\begin{code}
  evalLazy _ (Lambda t) = (Lambda t)
\end{code}

Error cases for call-by-name are a subset of error cases for
call-by-value.  Evaluating a free variable still generates an error
and an error occurring during evaluation of the first \texttt{App} term
is propagated back.  However, in call-by-name evaluation the second
term is not evaluated until necessary and the error case from
call-by-value does not occur.

\begin{code}
  evalLazy _ (Var x) = (CantEval (show (Var x)))
  evalLazy _ (App (CantEval x) t) = (CantEval x)
\end{code}

\section{Testing and Evaluation}

To test the interpreter, some functions from the book are provided
here.  The include the identity combinator, the omege combinator,
Church Boolean functions, some Church Numbers and the successor
function defined for Church numbers.

\subsection{Identity Combinator}

\begin{code}
  ident :: Term
  ident = (Lambda (Var 0))
\end{code}

\subsection{Church Booleans}

\begin{code}
  tru :: Term
  tru = (Lambda (Lambda (Var 1)))
  fls :: Term
  fls = (Lambda (Lambda (Var 0)))
\end{code}

\subsection{Church Numbers}

\begin{code}
  c0 :: Term
  c0 = (Lambda (Lambda (Var 0)))

  c1 :: Term
  c1 = (Lambda (Lambda (App (Var 1) (Var 0))))

  c2 :: Term
  c2 = (Lambda (Lambda (App (Var 1) (App (Var 1) (Var 0)))))

  scc :: Term
  scc = (Lambda
         (Lambda
          (Lambda (App (Var 1)
                   (App (Var 2)
                    (App (Var 1) (Var 0)))))))
\end{code}

\subsection{The Omega Combinator}

The omega combinator has the following form:

\begin{code}
  omega :: Term
  omega = (Lambda (App (Var 0) (Var 0)))
\end{code}

\subsection{Simple Test Cases}

This code has not been extensively tested, but some simple tests have
been performed.  Tests to try on Church Booleans include:

\begin{code}
  testTru :: Term
  testTru = (App (App tru ident) fls)

  testFls :: Term
  testFls = (App (App fls fls) ident)
\end{code}

A simple example of where call-by-value and call-by-name evaluation
differ occurs when the \texttt{omega} combinator is used as the false
argument to the \texttt{tru} combinator:

\begin{code}
  testCase1 :: Term
  testCase1 = (App (App tru ident) (App omega omega))
\end{code}

When evaluated with \texttt{evalValue}, this form does not terminate
because \texttt{(App omega omega)} must be evaluated before
\texttt{tru} can be evaluated.  When evaluated with \texttt{evalLazy},
the false case of the \texttt{tru} combinator is not evaluated because
it is not used.  Thus, the test case terminates as expected as the
identity combinator.

\bibliography{types,prog-langs}

\end{document}
