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
  the |InterprterLib| package to define simple interpreters.  This
  document assumes some knowledge of modular interpreter construction
  and refers to earlier interpreters written by the Uk'taad B'mal
  group.  These are available upon request.
\end{abstract}

\section{Introduction}

\begin{code}
  {-# OPTIONS -fglasgow-exts -fno-monomorphism-restriction #-}
  import InterpreterLib.Algebras
  import InterpreterLib.Functors
  import InterpreterLib.SubType
  import Monad
  import Control.Monad.Reader
\end{code}

\section{Example Language}

To demonstrate the use of the |InterpreterLib| definition
capabilities, we will define an interpreter for an |Integer| language
that implements some simple mathematical operations.

\subsection{The Value Space}

We start by defining a type for the interpreter's value space:

\begin{code}
  data Value
      = ValNum Int
      | ValLambda (ValueMonad -> ValueMonad)
  
  instance Show Value where
      show (ValNum x) = show x
      show (ValLambda _) = "<Function Value>"
\end{code}

There are two values in this language, integers and integer
functions.

\subsection{Defining AST Elements}

Next, we define types for each of the language's AST elements.  For
each AST element we define 5 elements: (i) the |Algebra| data type;
(ii) the |Term| data type; (iii) the |Functor| instance; (iv) the
|Algebra| instance; and (v) a helper function to create terms.  The
|Term| data structure and |Functor| instance remain unchanged from
previous modular interpreters.  The |Term| structure defines a
non-recursive type for representing terms while the |Functor| defines
a mechanism for folding operations onto the term structure.

The algebra function does not change from previous interpreters.  It
continues to provide a mapping from a term to a value.  The
distinction is the |Algebra| type requires three parameters - the
carrier set and the term type as before, plus the algebra structure
used for evaluation.  Note that the algebra is the instance of
|Algebra| while the algebra structure provides the definitions used by
the algebra.

The |apply| function defined for all |Algebra| instances takes the
place of |phi|.  It extracts what was |phi| from the algebra structure
and applies it.  Thus, |apply alg t| applies the interpretation
function from |alg| to the term |t|.  In affect, |phi = (apply alg)|.
One important difference should be noted.  Frequently, |phi| used
parameter matching to pull apart an argument and process its parts.
|apply| is virtually always called with the argument intact.

We will start with the definition for integer constants:

\begin{code}
  data AlgConst t = AlgC ((ExprConst t) -> t)

  data ExprConst e = EConst Int
                     deriving (Show,Eq)

  instance Functor ExprConst where
      fmap f (EConst x) = EConst x

  instance Algebra ExprConst AlgConst a where
      apply (AlgC f) x@(EConst _) = (f x)

  mkEConst = inn . sleft . EConst
\end{code}

Let's step through the definition for |Integer|.  |AlgConst| is the
algebra structure defining interpretation of constants.  It's only
parameter is a function that maps |ExperConst| instances over some
carrier set, |t| to |t|.  This is precisely the signature of |phi|
from earlier interpreters.  However, instead of using polymorphism to
find |phi|, we'll get it directly from the algebra when we invoke the
catamorphism.

|ExprConst| is the datatype associated with constants and is an
instance of |Functor|.  |ExprConst| is an instance of |Functor| and
|fmap| is defined in the canonical fashion to simply return the
constant it is passed.

|ExprConst| is also an instance of |Algebra|.  Here, the definition is
different because |apply| takes two arguments - an algebra structure
and a term - rather than one as it did in earlier implementations.  In
this case, |apply| first extracts the interpretation function, |f|,
from the algebra structure.  It then makes sure the term argument is
the correct type and associates it with |x|. With the evaluation
function and the term available, |apply| simply calls the
evaluation function on the term.

The |mkEConst| function is a helper function that constructs a
complete |ExprConstant| term.  Defining terms is a real pain with all
of the |Sum| and |Fix| cruft floating around.  I suspect that these
helper functions will need to be rewritten whenever the language
changes due to the structure of the |Sum|.  There may be a way around
this similar to the techniques used in earlier languages.

The addition and multiplication terms are defined similarly:

\begin{code}
  data AlgAdd t = AlgAdd {add :: (ExprAdd t) -> t,
                          sub :: (ExprAdd t) -> t}

  data ExprAdd e = EAdd e e
                 | ESub e e
                   deriving (Show,Eq)
                               
  instance Functor ExprAdd where
      fmap f (EAdd x y) = (EAdd (f x) (f y))
      fmap f (ESub x y) = (ESub (f x) (f y))

  instance Algebra ExprAdd AlgAdd a where
      apply alg x@(EAdd _ _) = (add alg x)
      apply alg x@(ESub _ _) = (sub alg x)

  mkEAdd x y = inn $ sright $ sleft $ EAdd x y
  mkESub x y = inn $ sright $ sleft $ ESub x y 
\end{code}

\begin{code}
  data AlgMult t = AlgMult {mult :: (ExprMult t) -> t,
                            divi :: (ExprMult t) -> t}

  data ExprMult e = EMult e e
                  | EDiv e e
                    deriving (Show,Eq)
                   
  instance Functor ExprMult where
      fmap f (EMult x y) = (EMult (f x) (f y))
      fmap f (EDiv x y) = (EDiv (f x) (f y))

  instance Algebra ExprMult AlgMult a where
      apply alg x@(EMult _ _) = (mult alg x)
      apply alg x@(EDiv _ _) = (divi alg x)

  mkEMult x y = inn $ sright $ sright $ sleft $ EMult x y
  mkEDiv x y = inn $ sright $ sright $ sleft $ EDiv x y
\end{code}

At this point we have elements of a simple language for arithmetic
with no variables or functions.  We can add lambdas, applications and
variables using techniques similar to those from earlier interpreters:

\begin{code}
  data ExprFun t
      = ELambda String TyValue t
      | EApp t t
      | EVar String
        deriving (Show,Eq)

  data AlgFun t = AlgFun {lam :: ExprFun t -> t,
                          app :: ExprFun t -> t,
                          var :: ExprFun t -> t}

  instance Functor ExprFun where
      fmap f (ELambda s ty t) = ELambda s ty (f t)
      fmap f (EApp t1 t2) = EApp (f t1) (f t2)
      fmap f (EVar s) = (EVar s)
  
  instance Algebra ExprFun AlgFun a where
      apply alg x@(EApp _ _) = (app alg x)
      apply alg x@(ELambda _ _ _) = (lam alg x)
      apply alg x@(EVar _) = (var alg x)

  mkEVar x = inn $ sright $ sright $ sright $ EVar x
  mkELambda x ty y = inn $ sright $ sright $ sright $ ELambda x ty y
  mkEApp x y = inn $ sright $ sright $ sright $ EApp x y
\end{code}

Note that the same five elements are defined for the collection of
lambda terms as for previous language elements.

The lambda implemented here uses a |Reader| monad to maintain
variables and their values in the execution environment as lambdas are
applied to values.  As each application is processes, the variable
being replaced is paired with the value specified by the application.
This is stored in the environment and used to determine the value of a
variable when it is referenced.

\subsection{Composing AST Elements}

The full language is now defined as the fixed point of the sum of
language components.  Here we have defined |:$:| as an infix form of
|Sum|.  However, the semantics is unchanged from previous
interpreters.  |TermType| is the sum of term definitions and
|TermLang| is the fixed point of the term definition.\footnote{|Fix|
and |:$:| are both defined in module |Functor|.}

\begin{code}
  type TermType = (ExprConst :$: (ExprAdd :$: (ExprMult :$: ExprFun)))

  type TermLang = Fix TermType
\end{code}

\section{Language Semantics}

We've now set up types for defining interpreters over this simple
language, but we've not defined a specific semantics for the language.
This is done by defining an algebra structure that provides
|apply| for each term AST and summing the result to form an algebra
for the complete language.

We start by defining types and functions for manipulating the
environment.  |ValueMonad| is the monad used to maintain the
environment as values are calculated for terms.  |Env| is the
environment an is defined as a single element record containing a list
|String|, |Value| pairs associating values with variables.
|lookupVal| and |addVal| are helper functions for looking up and
adding variable values to the environment.

\begin{code}
  type ValueMonad = Reader Env Value

  data Env = Env {variables::[(String,Value)]}

  lookupVal name env = lookup name (variables env)

  addVal b env = Env {variables=b:(variables env)}
\end{code}

\subsection{Semantic Elements}

Now we define helper functions that specify how each term type is
evaluated.  One function is defined for each AST construct.  These
definitions could easily be directly embedded in algebra structures
and not defined separately.  However, the algebra structure definition
is greatly simplified by using this approach.  They will each be
inserted into an algebra structure prior to their use.

\begin{code}
  phiConst (EConst x) = return (ValNum x)

  vPlus (ValNum x) (ValNum y) = ValNum (x + y)
  vSub (ValNum x) (ValNum y) = ValNum (x - y)

  phiAdd (EAdd x1 x2) = liftM2 vPlus x1 x2
  phiSub (ESub x1 x2) = liftM2 vSub x1 x2

  vMult (ValNum x) (ValNum y) = ValNum (x * y)
  vDiv (ValNum x) (ValNum y) = ValNum ((div) x y)

  phiMult (EMult x1 x2) = liftM2 vMult x1 x2
  phiDiv (EDiv x1 x2) = liftM2 vDiv x1 x2

  phiLambda (ELambda s _ t) =
      do { env <- ask
         ; return $ ValLambda (\v -> (do { v' <- v
                                         ; (local (const (addVal (s,v') env)) t)
                                         }))}
  phiApp (EApp x1 x2) =
      do { x1' <- x1
         ; case x1' of
           (ValLambda f) -> (f x2)
           _ -> error "Cannot apply non-lambda value"
         }
  phiVar (EVar s) = do { v <- asks (lookupVal s)
                       ; case v of
                         (Just x) -> return x
                         Nothing -> error "Variable not found"
                       }
\end{code}

Note the use of |liftM2| to evaluate |x1| and |x2| prior to applying
the actual evaluation function.  In effect, |x1| and |x2| are
evaluated in a |do| construct, then the specified function applied and
the result packaged back into the monad using |return|.  The
definition of |liftM2| is in the |Control.Monad| package, but is
repeated at the end of this file for documentation purposes.

\subsection{Composign Semantic Elements}

The full term algebra is formed by creating algebra structures for
each term from the definitions above and summing those definitions
together.  |AlgC|, |AlgAdd|, |AlgMult| and |AlgFun| take a function
and build an algebra structure around it.  This is what the data type
definitions earlier are for.  |@+@| is an infix Sum operation for
algebra structures.  This works the same way as the term sum to
combine algebra structures into a single structure.\footnote{I believe
  the order structure of this sum and the term sum must be the same.
  Specifically, |AlgCost| and |ExprConst| are both first; |AlgAdd| and
  |ExprAdd| are both second; and so forth.  I have not tested this
  assumption, but it would make very good sense to do it this way.}

\begin{code}
  termAlg = (AlgC phiConst)
            @+@ (AlgAdd phiAdd phiSub)
            @+@ (AlgMult phiMult phiDiv)
            @+@ (AlgFun phiLambda phiApp phiVar)
\end{code}

The |evalFun| function composes |runReader| and |cata| to define
evaluation.  The heart of this function is the polytypic fold, or
catamorphism.  |(cata termAlg)| instantiates the |cata| function with
the evaluation algebra.  When applied to a term, it will produce a
|ValueMonad| that is then evaluated by |runReader|.  

\begin{code}
  evalFun = runReader . (cata termAlg)
\end{code}

An initial value for the environment must be provided to |evalFun| for
the reader to evaluate completely.  To evaluate |term1| starting with
an empty environment, execute the following:

\begin{spec}
  (evalFun term1) Env {variables=[]}
\end{spec}

\section{A Second Interpreter}

Now let's have some fun and define a different evaluation function for
this language.  If all is well, we should be able to define a new
algebra structure, use the same sum and evaluate the language over a
different carrier set.  For this experiment, we'll use the simple
odd/even carrier set.

First define the odd/even data type and some helper functions.
Probably could use instances and continue to use + and *, but that's
more than we really want to do here.

\begin{code}
  data OE = Odd | Even deriving (Show,Eq)
 
  oePlus :: OE -> OE -> OE
  oePlus Odd Odd = Even
  oePlus Odd Even = Odd
  oePlus Even Odd = Odd
  oePlus Even Even = Even

  oeTimes :: OE -> OE -> OE
  oeTimes Odd Odd = Odd
  oeTimes Odd Even = Even
  oeTimes Even Odd = Even
  oeTimes Even Even =  Even
\end{code}

Second, define the evaluation functions that we'll use with |apply|.
For these definitions, we'll define |alpha|, the abstraction function
for constant values, and define the various evaluation functions using
|alpha| when necessary.  This will allow us to check soundness later.
Note that we have to be careful about the name space and use unique
names here.

\begin{code}
  data AbsValue
      = AbsValNum OE
      | AbsValLambda (AbsValueMonad -> AbsValueMonad)

  instance Show AbsValue where
      show (AbsValNum v) = show v
      show (AbsValLambda _) = "<Abstract Function Value>"

  instance Eq AbsValue where
      (==) (AbsValNum x) (AbsValNum y) = (x == y)
      (==) (AbsValLambda x) (AbsValLambda y) = error "Cannot compare functions."

  type AbsValueMonad = Reader AbsEnv AbsValue

  data AbsEnv = AbsEnv {absVariables::[(String,AbsValue)]}

  lookupAbsVal name env = lookup name (absVariables env)

  addAbsVal b env = AbsEnv {absVariables=b:(absVariables env)}
\end{code}

\begin{code}
  alpha x = AbsValNum $ if (odd x) then Odd else Even
\end{code}

Now define evaluation functions for each element of the AST:

\begin{code}
  phi1Const (EConst x) = return (alpha x)

  aPlus (AbsValNum x) (AbsValNum y) = AbsValNum (oePlus x y)
  phi1Add (EAdd x1 x2) = liftM2 aPlus x1 x2
  phi1Sub (ESub x1 x2) = liftM2 aPlus x1 x2

  aTimes (AbsValNum x) (AbsValNum y) = AbsValNum (oeTimes x y)
  phi1Mult (EMult x1 x2) = liftM2 aTimes x1 x2 
  phi1Div (EDiv x1 x2) = liftM2 aTimes x1 x2 

  phi1Lambda (ELambda s _ t) =
      do { env <- ask
         ; return $ AbsValLambda (\v -> (do { v' <- v
                                            ; (local (const (addAbsVal (s,v') env)) t)
                                            }))}

  phi1App (EApp x1 x2) =
      do { x1' <- x1
         ; case x1' of
           (AbsValLambda f) -> (f x2)
           _ -> error "Cannot apply non-lambda value"
         }

  phi1Var (EVar s) = do { v <- asks (lookupAbsVal s)
                       ; case v of
                         (Just x) -> return x
                         Nothing -> error "Variable not found"
                       }
\end{code}

Finally, create the algebra structures for each term and sum them
together to create the term algebra.  The helper function |evalPar| is
identical to |evalFun| defined previously except it uses the abstract
interpreter.

\begin{code}
  term1Alg = (AlgC phi1Const)
           @+@ (AlgAdd phi1Add phi1Sub)
           @+@ (AlgMult phi1Mult phi1Div)
           @+@ (AlgFun phi1Lambda phi1App phi1Var)

  evalPar = runReader . (cata term1Alg)
\end{code}

We're done.  Now we can use the same terms as before to test the new
abstract interpreter.

\section{Type Checking as Interpretation}

Now let's have even more fun.  Using the same algebraic structure, we
can define a type checker for our tiny language by once again defining
a new value space and associated |phi| functions.  We'll leave it to
the reader to determine what's going on here.

\begin{code}
  data TyValue
      = TyInt
      | TyValue :->: TyValue
        deriving (Eq,Show)

  data Gamma = Gamma {gamma::[(String,TyValue)]}

  lookupTy name gam = lookup name (gamma gam)

  addBinding b gam = Gamma {gamma=b:(gamma gam)}

  type TyMonad = Reader Gamma TyValue
\end{code}

\begin{code}
  tyConst (EConst x) = return TyInt

  tPlus TyInt TyInt = TyInt
  tPlus (_:->:_) _ = error "Cannot add function value"
  tPlus _ (_:->:_) = error "Cannot add function value"

  tSub TyInt TyInt = TyInt
  tSub (_:->:_) _ = error "Cannot subtract function value"
  tSub _ (_:->:_) = error "Cannot subtract function value"

  tyAdd (EAdd x1 x2) = liftM2 tPlus x1 x2
  tySub (ESub x1 x2) = liftM2 tSub x1 x2

  tMult TyInt TyInt = TyInt
  tMult (_:->:_) _ = error "Cannot multiply function value"
  tMult _ (_:->:_) = error "Cannot multiply function value"

  tDiv TyInt TyInt = TyInt
  tDiv (_:->:_) _ = error "Cannot divide function value"
  tDiv _ (_:->:_) = error "Cannot divide function value"

  tyMult (EMult x1 x2) = liftM2 tMult x1 x2
  tyDiv (EDiv x1 x2) = liftM2 tDiv x1 x2

  tyLambda (ELambda s ty t) =
      do { g <- ask
         ; t' <- (local (const (addBinding (s,TyInt) g)) t)
         ; return (ty :->: t')
         }

  tyApp (EApp x1 x2) =
      do { x1' <- x1
         ; x2' <- x2
         ; case x1' of
           (t1 :->: t2) -> if (t1 == x2')
                           then (return t2)
                           else (error "Input parameter of wrong type")
           _ -> error "Cannot apply non-lambda value"
         }

  tyVar (EVar s) = do { v <- asks (lookupTy s)
                        ; case v of
                        (Just x) -> return x
                        Nothing -> error "Variable not found"
                      }

  tyAlg = (AlgC tyConst)
            @+@ (AlgAdd tyAdd tySub)
            @+@ (AlgMult tyMult tyDiv)
            @+@ (AlgFun tyLambda tyApp tyVar)

  typeof = runReader . (cata tyAlg)
\end{code}

That's it.  The type checker in just over a page.  Not bad at all.

\section{A Moment of Soundness}

One more interesting thing before we quit is evaluating soundness of
the abstract interpretation.  Specifically, does applying the
abstraction function after interpretation result in the same value as
abstract interpretation?  If so, then $\alpha(\phi_c(m))=\phi_a(m)$
where $\phi_c$ is concrete interpretation and $\phi_a$ is abstract
interpretation.

\begin{code}
  soundTest c a alpha x = do { x' <- cata c x
                             ; x'' <- cata a x
                             ; return (x'' == (alpha x'))
                             }

  sound x = case v of
            (ValNum a)  -> case av of
                           c@(AbsValNum b) -> alpha a == c
                           _ -> False
            (ValLambda _) -> case av of
                             (AbsValLambda _) -> error "Cannot compare functions"
                             _ -> False
      where v=(evalFun x Env {variables=[]});
            av=(evalPar x AbsEnv {absVariables=[]})
\end{code}

\section{Testing Functions}

The remaining definitions are helper functions for creating terms and
calling |cata| to perform evaluation.  All are worth looking at to see
the structure of terms and to see the use of monads during evaluation.

\begin{code}
  sright = S . Right

  sleft = S . Left

  term1 = mkEConst 1
  term2 = mkEAdd term1 term1
  term3 = mkEMult term2 term2
  term4 = mkESub term1 term1
  term5 = mkEDiv term1 term1
  term6 = mkEVar "x"
  term7 = mkELambda "x" TyInt term6
  term8 = mkEApp term7 term1
  term9 = mkELambda "x" TyInt (mkEAdd term6 term6)
  term10 = mkEApp term9 term1
  term11 = mkELambda "x" TyInt (mkELambda "y" TyInt (mkEAdd term1 term1))
  term12 = mkEApp (mkEApp term11 term1) term1

  emptyG = Gamma {gamma=[]}
  emptyE = Env {variables=[]}
  emptyAE = AbsEnv {absVariables=[]}
\end{code}

I always forget the definitions of |liftM| and |liftM2|, so I'll
include them here in a specification block for reference.

\begin{spec}
  -- Defined in Control.Monad
  liftM2 f m1 m2 = do x1 <- m1
                      x2 <- m2
                      return (f x1 x2)

  liftM f m1 = do x <- m
                  return (f x)
\end{spec}

This does not work, so uncomment at your own risk.  It's supposed to
mimic SubSum from LangUtils, but insists on a fully instantiated type
instead of a type constructor.

\begin{spec}
  toTmLang :: (SubType f TermType) => f TermLang -> TermLang
  toTmLang = inj
\end{spec}

\end{document}
