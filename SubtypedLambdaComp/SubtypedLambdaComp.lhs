\documentclass[10pt]{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty

%format e1 = "\Varid{e}_{1}"
%format e2 = "\Varid{e}_{2}"
%format e3 = "\Varid{e}_{3}"
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
%format Ty = "\Varid{T}"_{<:}
%format TyBase = "\Varid{T}_{\lambda}"
%format TyTpl = "\Varid{T}_{(a,b)}"
%format TyRec = "\Varid{T}_{\{a:T\}}"
%format Expr = "\Varid{Expr}_{<:}"
%format TermBase = "\Varid{T}_{\lambda}"
%format TermTpl = "\Varid{T}_{(a,b)}"
%format TermRec = "\Varid{T}_{\{a:T\}}"
%format Expr1 = "\Varid{Expr}_1"
%format Expr2 = "\Varid{Expr}_2"
%format Expr3 = "\Varid{Expr}_3"
%format eval = "\Varid{eval}_{<:}"
%format eval1 = "\Varid{eval}_1"
%format eval2 = "\Varid{eval}_2"
%format eval3 = "\Varid{eval}_3"
%format eval4 = "\Varid{eval}_4"
%format typeof = "\Varid{typeof}_{<:}"
%format typeof1 = "\Varid{typeof}_1"
%format typeof2 = "\Varid{typeof}_2"
%format typeof3 = "\Varid{typeof}_3"
%format typeof4 = "\Varid{typeof}_4"
%format omega = "\Varid{\Omega}"
%format proj = "\Varid{\pi}"
%format proj1 = "\Varid{\pi}_1"
%format proj2 = "\Varid{\pi}_2"
%format proj3 = "\Varid{\pi}_3"
%format proj4 = "\Varid{\pi}_4"
%format inj = "\Varid{\sigma}"
%format inj1 = "\Varid{\sigma}_1"
%format inj2 = "\Varid{\sigma}_2"
%format inj3 = "\Varid{\sigma}_3"
%format inj4 = "\Varid{\sigma}_4"
%format phi = "\Varid{\phi}"
%format phi1 = "\Varid{\phi_1}"
%format phi2 = "\Varid{\phi_2}"
%format phi3 = "\Varid{\phi_3}"
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

\title{Composable Interpreters: Subtyped Lambda Calculus Examples}
\author{\emph{Uk'taad B'mal} \\
  The University of Kansas - ITTC \\
  2335 Irving Hill Rd, Lawrence, KS 66045 \\
  \texttt{lambda@@ittc.ku.edu}}

\begin{document}

\maketitle

\section{Introduction}

The objective of this exercise is to write a collection of composable
interpreters for simply typed lambda calculus with subtyping
($\lambda_{<:}$) and add records and tuples as defined in \emph{Types
and Programming Languages}~\cite{Pie02a}, Chapters 15 and 16.  The
real goal is to understand the applicability of the modular
interpreter techniques we have been exploring (including
Duponcheel~\cite{} and Liang~\cite{}) and semantic definition
techniques using |fold| as proposed by Hutton~\cite{}.

\begin{code}
  module SubtypedLambdaComp ( TermBase(..),TermRec(..),TermTpl(..))
      where
  import Control.Monad.Error
  import Data.List
\end{code}

The |SubtypedLambdaComp| module provides the basic definitions for
manipulating simply typed lambda calculus expressions with subtyping
using composable interpreters.  As indicated by the module name, a
monad is used to implement the evaluation and type checking processes.
The data type representing terms and types is first defined, followed
by the type checking function, subtyping function, and the evaluation
function.

\subsection{Type Classes and Properties}

%  data Retval a = 
%      Value a | Error String
%      deriving (Eq,Show)
%	       
%  instance Monad Retval where
%      Error s >>= k = Error s
%      Value a >>= k = k a 
%      return = Value
%      fail = Error

The type |Retval| defines a synonym for the return type of evaluation
fucntions.  Specifically, it defines an encapcsulation of |a|, the
term value type, in an error monad for returning error messages.

\begin{code}
  type Retval a = ErrorT String IO a
\end{code}

The type class |Termable| defines properties that must be exhibited by
all terms. If something is a term, then it has a |value| predicate and
can be evaluated using the |eval| function.  The |value| predicate
defines when a term is an element of the value space.  If a term, |t|,
is a value then the result of evaluating the term should be |t|.  The
default evaluation function simply says there is no defined evaluation
function

\begin{code}
  class Termable a where
      value :: a -> Bool
      eval :: a -> Retval a
      eval t = if value t
	       then return t
	       else fail "No evaluation function defined for term."
\end{code}

If something is typeable, then it must be a term and its type can be
found.  The |typeof| function deterines the type of the term specified
by its argument.

\begin{code}
  class Termable a => Typeable a b where
      typeof :: a -> b
\end{code}

If one type is a |Subtype| of another, then an injection function maps
the subtype to the supertype and a projection function maps the
supertype to |Maybe| subtype.  The latter occurs because not every
supertype witness has an corresponding witness in the subtype.

\begin{code}
  class Subtype a b where
      inj :: a -> b
      proj :: b -> Maybe a
\end{code}

\subsection{Base Types and Terms}

A cosntructed type, |TyBase|, is defined to represent the types of
base terms.  It is parameterized over |ty| allowing other types to be
encapsulated in the base type.

\begin{code}
  data TyBase ty
      = TyBool
      | TyTop
      | TyArr ty ty
      deriving (Eq,Show)
\end{code}

A constructed type, |TermBase|, is defined to represent the abstract
syntax for terms.  |TermBase| is parameterized over |te| and |ty|
representing other terms and types that can be referenced by
|TermBase|.

\begin{code}
  data TermBase te ty
      = TmTrue | TmFalse
      | If te te te
      | Var Int
      | Lambda ty te
      | AppV te te
      | AppN te te
      deriving (Eq,Show)
\end{code}      

The |value| function defines a predicate that specifies |TermBase|
terms that are values.

|TermBase| defines a term and is thus an instance of |Termable|.  By
definition, |TmTrue|, |TmFalse|, and |Lambda| forms are constructors
for values in |TermBase|.  |If|, |AppV| and |AppN| are forms that can
be evaluated to simpler forms and are thus not values.  This provides
a definition for the |value| function:

\begin{code}
  instance Termable (TermBase te ty) where
      value TmTrue = True
      value TmFalse = True
      value (Lambda _ _) = True
      value _ = False
\end{code}

Terms defined in |TermBase te ty| should have types defined in |TyBase
ty|.  However, to support what will become a recursive sturcture,
|TyBase ty| sbhould also be a subtype of |ty|.  This will allow the
construction of terms from other terms to result in types build from
other types.  This is particulary important for the |Lambda|
construct.  Unfortunately, it's breaking the type checker in very
serious ways right now.  The instance signature is attempting to say
that if |TyBase ty| is a subtype of |ty|, then |Termbase te ty| is
typeable in |ty|.  But do we know that |Termbase te ty| is typeable in
|ty|?  It is necessary for the instantiation, but not implied by the
instantiation.

\begin{code}
  -- Subtype (TyBase ty) ty => inj :: (TyBase ty) -> ty
  --                        => proj :: ty -> Maybe (TyBase ty)
  -- Typeable (Termbase te ty) ty => typeof :: (Termbase te ty) -> ty

  instance (Subtype (TyBase ty) ty) => Typeable (TermBase te ty) ty where
      typeof TmTrue = inj  TyBool
      typeof TmFalse = inj TyBool
      typeof (If be e1 e2) = if ((typeof be) == TyBool)
			     then inj TyBool
			     else inj TyBool
--	  do{ (typeof (proj be))
--	    ; return (inj tbe)
--	    ; if tbe == Nothing
--	      then fail "Type Error - If expression not boolean"
--	      else fail "Type Error - If arms of incompatible types"
--	    }
--	  if (proj tbe) == TyBool && te1 == te2
--	  then (typeof e1)
--	  else fail "Type Error - If arms of incompatible types"
--	      where tbe = (typeof be)
--		    ; te1 = typeof e1
--		    ; te2 = typeof e2
      typeof (Lambda tyv t) = inj (TyArr tyv (typeof t))

\end{code}

\subsection{Tuple and Record Types and Terms}

\begin{code}
  newtype TyTpl ty = TyTpl [ty] deriving (Eq,Show)
  newtype TyRec ty = TyRec [(String,ty)] deriving (Eq,Show)
\end{code}

\begin{code}
  data TermTpl te
      = Tpl [te]
      | ProjTpl (TermTpl te) Int
	deriving Eq

  data TermRec te
      = Rec [(String,te)]
      | ProjRec (TermRec te) String
	deriving Eq
\end{code}

\begin{code}
  instance Termable (TermTpl te) where
      value (Tpl _) = True
      value _ = False

  instance (Typeable te ty) => Typeable (TermTpl te) (TyTpl ty) where
      typeof (Tpl l) = (TyTpl (map typeof l))

  instance Termable (TermRec te) where
      value (Rec _) = True
      value _ = False

  instance (Typeable te ty) => Typeable (TermRec te) (TyRec ty) where
      typeof (Rec l) = (TyRec (map ( \(s,t) -> (s,(typeof t))) l))
\end{code}

\subsection{Duponcheel Composition}

\begin{code}
  newtype Ty =
      InTy (Either (TyBase Ty) (Either (TyRec Ty) (TyTpl Ty)))
      deriving Eq

  outTy (InTy x) = x

  newtype Term =
      InTe (Either (TermBase Term Ty) (Either (TermRec Term) (TermTpl Term)))
      deriving Eq

  outTerm (InTe x) = x

--  instance Termable Term where
--      value = either value (either value value)

\end{code}

\bibliography{prog-langs}

\end{document}
