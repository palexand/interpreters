\documentclass[10pt]{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty

%format phi = "\Varid{\phi}"
%format phi1 = "\Varid{\phi_1}"
%format phi2 = "\Varid{\phi_2}"
%format phi3 = "\Varid{\phi_3}"
%format eval1 = "\Varid{eval}_1"
%format eval2 = "\Varid{eval}_2"
%format eval3 = "\Varid{eval}_3"
%format eval4 = "\Varid{eval}_4"
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
%format test10 = "\Varid{test}_{10}"

\usepackage{fullpage}
\usepackage{alltt}

\bibliographystyle{plain}

\parskip=\medskipamount
\parindent=0pt

\title{A Haskell Companion for ``Fold and Unfold for Program Semantics''}
\author{\emph{Uk'taad B'mal} \\
  The University of Kansas - ITTC \\
  2335 Irving Hill Rd, Lawrence, KS 66045 \\
  \texttt{lambda@@ittc.ku.edu}}

\begin{document}

\maketitle

\begin{abstract}

  This document is a primer to accompany the paper ``Fold and Unfold
  for Program Semantics'' by Graham Hutton.  It attempts to
  re-implement in \texttt{Haskell} the interpreters written in
  \texttt{Gopher}.

\end{abstract}

\section{Introduction}

%include FoldExpression.lhs

%include UnfoldExpression.lhs

%include Catamorph.lhs

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
   lhs2TeX --math Hutton.lhs > Hutton.tex
\end{alltt}

and run \LaTeX on the result.  The individual interpreters cannot be
transformed to \LaTeX directly.

\end{document}
