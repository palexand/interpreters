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

\title{Monadic Typed Lambda Calculus Interpreter}
\author{Perry Alexander \\
  ITTC - The University of Kansas \\
  2335 Irving Hill Rd \\
  Lawrence, KS 66045 \\
  \texttt{alex@@ittc.ku.edu}}

\begin{document}

\maketitle

\section{Introduction}

The objective of this project is to write an interpreter for an
extended simply typed lambda calculus ($\lambda_\rightarrow$) based on
definitions from \emph{Types and Programming Languages}~\cite{Pie02a},
Chapter 8, Figure 8-1 and Chapter 9, Figure 9-1.  We will enhance the
basic language to include integers and integer sum and difference in
addition to the basic operations.  The definition of the abstract
syntax provides the following forms for $\lambda_\rightarrow$ terms,
values and types in:

\begin{eqnarray*}
  t & \isa & x \ora v \ora \lambda x:T.t \ora t\; t \ora \PLUS t\; t \ora \MINUS t\;
  t \\
  v & \isa & \lambda x:T.t \ora \mathcal{I} \ora \mathtt{true} \ora \mathtt{false}\\
  T & \isa & \BOOL \ora \INT \ora T \rightarrow T
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
  {t_1\rightarrow t_1^{'}}}
\]

\[\vcenter{\infer[\textsc{E-Plus1}]
  {\PLUS t_1^{'}\; t_2^{'}}
  {t_1\rightarrow t_1^{'} & t_2\rightarrow t_2^{'}}}
\]

\[\vcenter{\infer[\textsc{E-Plus2}]
  {\mathcal{I}_1 + \mathcal{I}_2}
  {\PLUS \mathcal{I}_1\; \mathcal{I}_2}}
\]

\[\vcenter{\infer[\textsc{E-Minus1}]
  {\MINUS t_1^{'}\; t_2^{'}}
  {t_1\rightarrow t_1^{'} & t_2\rightarrow t_2^{'}}}
\]

\[\vcenter{\infer[\textsc{E-Minus2}]
  {\mathcal{I}_1 - \mathcal{I}_2}
  {\MINUS \mathcal{I}_1\; \mathcal{I}_2}}
\]

where $\mathcal{I}$ is any constant integer value.

The following typing rules that define the type inference function:

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

\[\vcenter{\infer[\textsc{T-True}]{\TRUE:\BOOL}{}}\]

\[\vcenter{\infer[\textsc{T-False}]{\FALSE:\BOOL}{}}\]

\[\vcenter{\infer[\textsc{T-Plus}]
  {\PLUS t_1\; t_2 : \INT}
  {t_1 : \INT & t_2 : \INT}}
\]

\[\vcenter{\infer[\textsc{T-Minus}]
  {\MINUS t_1\; t_2 : \INT}
  {t_1 : \INT & t_2 : \INT}}
\]

Our objective is to: (i) define a data structure for representing
$\lambda_\rightarrow$ terms embodying the abstract syntax; (ii) a type
derivation function for $\lambda_\rightarrow$ terms embodying the type
rules; and (iii) an evaluation function for $\lambda_\rightarrow$
terms embodying the evaluation rules.

%include TypedLambdaExtendedAST.lhs

%include TypedLambdaExtendedEnv.lhs

%include TypedLambdaExtendedTypesT.lhs

%include TypedLambdaExtendedEval.lhs

%include TypedLambdaExtendedInterpreter.lhs

\bibliography{prog-langs}

\end{document}
