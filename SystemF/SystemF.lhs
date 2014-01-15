\documentclass[10pt]{article}

\usepackage{url}
\usepackage{proof}
\usepackage{fullpage}

%include lhs2TeX.fmt
%include LangUtils.fmt
%include lhs2TeX.sty

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
\newcommand{\INT}{\ensuremath{\mathtt{\; Int \;}}}
\newcommand{\PLUS}{\ensuremath{\mathtt{ plus \;}}}
\newcommand{\MINUS}{\ensuremath{\mathtt{ sub \;}}}

\title{System F Interpreter}
\author{Perry Alexander \\
  ITTC - The University of Kansas \\
  2335 Irving Hill Rd \\
  Lawrence, KS 66045 \\
  \texttt{alex@@ittc.ku.edu}}

\begin{document}

\maketitle

\section{Introduction}

The objective of this project is to write an interpreter for System F
($\rightarrow\forall$) based on definitions from \emph{Types and
Programming Languages}~\cite{Pie02a}, Chapter 23, Figure 23-1.  We
will enhance the basic language to include integers and integer sum
and difference in addition to the basic operations.

Our objective is to: (i) define a data structure for representing
$\rightarrow\forall$ terms embodying the abstract syntax; (ii) a type
derivation function for $\rightarrow\forall$ terms embodying the type
rules; and (iii) an evaluation function for $\rightarrow\forall$
terms embodying the evaluation rules.

%include SystemFAST.lhs

%include SystemFEnv.lhs

%include SystemFTypesT.lhs

%include SystemFEval.lhs

%include SystemFInterpreter.lhs

\bibliography{prog-langs}

\end{document}
