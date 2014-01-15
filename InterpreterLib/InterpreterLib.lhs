\documentclass[10pt]{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty

%format alpha = "\Varid{\alpha}"
%format gamma = "\Varid{\gamma}"
%format phi = "\Varid{\phi}"
%format phi1 = "\Varid{\phi_1}"
%format phi2 = "\Varid{\phi_2}"
%format phi3 = "\Varid{\phi_3}"
%format eval1 = "\Varid{eval}_1"
%format eval2 = "\Varid{eval}_2"
%format eval3 = "\Varid{eval}_3"
%format Expr1 = "\Varid{Expr}_1"
%format Expr2 = "\Varid{Expr}_2"
%format Expr3 = "\Varid{Expr}_3"
%format E1 = "\Varid{E}_1"
%format E2 = "\Varid{E}_2"
%format E3 = "\Varid{E}_3"
%format evalE = "\Varid{eval}_E"
%format mapE = "\Varid{map}_E"
%format phiE = "\Varid{\phi}_E"
%format test0 = "\Varid{test}_0"
%format test1 = "\Varid{test}_1"
%format test2 = "\Varid{test}_2"
%format test3 = "\Varid{test}_3"
%format test4 = "\Varid{test}_4"
%format test5 = "\Varid{test}_5"
%format test6 = "\Varid{test}_6"
%format test7 = "\Varid{test}_7"
%format test8 = "\Varid{test}_8"
%format test9 = "\Varid{test}_9"
%format term10 = "\Varid{term}_{10}"
%format term0 = "\Varid{term}_0"
%format term1 = "\Varid{term}_1"
%format term2 = "\Varid{term}_2"
%format term3 = "\Varid{term}_3"
%format term4 = "\Varid{term}_4"
%format term5 = "\Varid{term}_5"
%format term6 = "\Varid{term}_6"
%format term7 = "\Varid{term}_7"
%format term8 = "\Varid{term}_8"
%format term9 = "\Varid{term}_9"
%format term10 = "\Varid{term}_{10}"

\usepackage{fullpage}
\usepackage{alltt}

\bibliographystyle{plain}

\parskip=\medskipamount
\parindent=0pt

\title{The InterpreterLib Explicit Algebra Package}
\author{\emph{Uk'taad B'mal} \\
  The University of Kansas - ITTC \\
  2335 Irving Hill Rd, Lawrence, KS 66045 \\
  \texttt{lambda@@ittc.ku.edu}}

\begin{document}

\maketitle

\begin{abstract}

The use of \emph{composable interpreters} has proven to be useful in
the development of language parsers in our group.  However, some
aspects of techniques used in papers from the literature do not scale
well to larger projects.  More specifically, using polymorphism to
select |phi| as is done in the |LangUtils| library will not work when
multiple interpreters exist in the same environment.  Furthermore,
having instances of |Algebra| be opaque causes serious problems when
we start looking at composing algebras.

\end{abstract}

\section{Introduction}

The |InterpreterLib| libraries are a collection of support packages
for writing \emph{composable interpreters} using \emph{explicit
  algebras}.  The term composable describes interpreters that are
composed of modules defining interpreters for language components.
Instead of writing a monolithic interpreter, we write individual
components and assemble those components as needed for a specific
language.  The literature describes several approaches for writing
composable
interpreters~\cite{Liang:96:Modular-Denotat,Espinosa:95:Semantic-Lego,Duponcheel::Using-catamorph,Jones:93:Composing-Monad,Steele:94:Building-interp}.
All of them share the construction and integration of interpreter
components.\footnote{The Lambda Group and SLDG lab have reports
  documenting the Hutton and Duponcheel approaches as well as example
  interpreters.  The |LangUtils| module is also worth looking at for
  other examples.}

The approach we take in our interpreters is writing \emph{functors}
and \emph{semantic algebras}.  A functor is simply a specialized fold
for a language construct.  Recall that fold is a mechanism for
recursively applying a function to a composite data structure and
accumulating results.  Functors for language elements ``push''
functions into language constructs.  Each functor defines a function,
|fmap|, that performs this function.  For example, |fmap| over an |if|
construct might have the following definition:

\begin{spec}
  fmap g (IfExpr c t f) = (IfExpr (g c) (g t) (g f))
\end{spec}

Thus, if we wanted to apply an interpretation function or some
transformation function to a specific |IfExpr| we simply call |fmap fun|
on the expression.  If we define a functor for each language
construct, we can fold a function into any term we might write.

A semantic algebra does exactly what its name implies by defining a
semantics for each language construct.  Algebras for language elements
define how they are evaluated.  Each algebra defines a function,
traditionally called |phi|, that maps its associated language construct
to a value.  Using |fmap| to fold |phi| onto a composite language
structure implements an interpreter for the language.\footnote{In the
  |InterpreterLib| modules, we use explicit algebras where |phi| is
  replaced by |apply|, but the principle is similar.}
For example, |phi| for an |if| construct might have the following
definition:

\begin{spec}
  phi (IFExpr c t f) = do { c' <- c
                          ; return $ if c' == ETrue then t else f
                          }
\end{spec}

The general idea is that we can write new semantic algebras and reuse
functors to quickly generate new interpreters.  What |InterpreterLib|
does that |LangUtils| does not is provides a way to explicitly specify
the algebra used by an interpreter.  Further, the algebra structure
defined is a \texttt{Haskell} data structure that can be manipulated
like any other structure.  Thus, defining traditional functors between
algebras as well as algebra combinators is now possible.

\section{Functors}
%include Functors.lhs

\section{Algebras}
%include Algebras.lhs

%%\section{SubType}
%% %include SubType.lhs

\section{Modules}
%include Modules.lhs

\section{Sample Interpreter}
%include Sample.lhs

\section{Term Libraries}

Included with the base |InterpreterLib| system are a collection of
Imodules for building various terms and data structures.  These
Ilibraries simply provide boilerplate for structuring algebras.  They
Ido note define semantics for the abstract syntax structures they
Idefine.  The libraries are intended to serve as both documentation
Iand building blocks for interpreters.

\subsection{Arithmetic Terms}
%include Terms/ArithTerm.lhs

\subsection{Fixed Point Term}
%include Terms/FixTerm.lhs

\subsection{IO Terms}
%include Terms/IOTerm.lhs

\subsection{If Term}
%include Terms/IfTerm.lhs

\subsection{Reference Terms}
%include Terms/ImperativeTerm.lhs

\subsection{Lambda Terms}
%include Terms/LambdaTerm.lhs

\subsection{Let Term}
%include Terms/LetTerm.lhs

\subsection{Procedure Term}
%include Terms/ProcTerm.lhs

\subsection{RAL Term}
%include Terms/RALTerm.lhs

\subsection{Record Terms}
%include Terms/RecordTerm.lhs

\subsection{String Terms}
%include Terms/StringTerm.lhs

\subsection{Sum Terms}
%include Terms/SumTerm.lhs

\subsection{Unit Term}
%include Terms/UnitTerm.lhs

\subsection{Variable Terms}
%include Terms/VarTerm.lhs

\section{Usage}

This file is a template for transforming literate script into \LaTeX
and is not actually a \texttt{Haskell} interpreter implementation.
Each section in this file is a separate module that can be loaded
individually for experimentation.

Note that the interpreters have been developed under GHC and some
require turning on the Glasgow Extensions.  Your mileage may vary if
you're using HUGS.

To build a \LaTeX document from the interpreter files, use:

\begin{alltt}
   lhs2TeX --math InterpreterLib.lhs > InterpreterLib.tex
\end{alltt}

and run \LaTeX on the result.

\bibliography{sldg.bib}

\end{document}
