\section{Second Try}

Module |SecondTry| presents a Haskell encoding of Duponcheel's Second
Try and building a modular interpreter:

\begin{code}
  module SecondTry where
\end{code}

Again, we define data structures for |E1| representing the abstract
syntax of numbers and addition, and |E2| representing the abstract
syntax of division.  Unlike First Try, these data structures are
separate types::

\begin{code}
  data E1 x
      = Num Int
      | Add x x
	deriving (Eq,Show)

  data E2 x = Dvd x x deriving (Eq,Show)
\end{code}

The difficult with expressions as defined in First Try is their
recursive nature.  Because of this, the single data structure
representing expression must be modified each time the language is
updated.  Here, we use |E1| and |E2| to define expressions that are not
directly recursive.  The data constructors |In1| and |In2| prevent
general recursion in the data types:

\begin{code}
  data Expr1 = In1 (E1 Expr1) deriving (Eq,Show)

  data Expr2 = In2 (E2 Expr2) deriving (Eq,Show)
\end{code}

Semantic functions for each of the operations remain unchanged and
will remain unchanged in all our interpreters:

\begin{code}
  num = id         -- Num1 Int
  add x y = x+y    -- Add1 Expr1 Expr1
  dvd x y = if y==0 then error "Divide by zero" else x `div` y
\end{code}

%% Working here...

Two helper functions are used to define evaluation functions for each
of the data types.  A |map| function distributes a function across an
expression.  Specifically, if |g| is a function, then mapping |g| onto
|Add x y| is equal to |Add (g x) (g y)|.  The |map| function will be
used to map |eval| functions onto language terms.  The |phi| functions
map syntax to semantics.  Each |phi| function maps a term to its
semantic interpretation.

\begin{code}
  map1 :: (x->y) -> (E1 x -> E1 y)
  map1 g (Num n) = Num n
  map1 g (Add e f) = (Add (g e) (g f))

  map2 :: (x->y) -> (E2 x -> E2 y)
  map2 g (Dvd e f) = (Dvd (g e) (g f))

  phi1 :: E1 Int -> Int
  phi1 (Num n) = num n
  phi1 (Add e f) = e `add` f

  phi2 :: E2 Int -> Int
  phi2 (Dvd e f) = e `dvd` f
\end{code}

An evaluation function becomes mapping the evaluation function onto a
term and semantically interpreting the result.  The functions |eval1|
and |eval2| separately evaluate expressions |Expr1| and |Expr2| and
are included as examples of writing evaluation functions. They will
not be used to write the composite evaluation function for both terms.
Instead, |map| and |psi| functions will be composed to form a new
evaluation function.

\begin{code}
  eval1 :: Expr1 -> Int
  eval1 (In1 e1) = phi1 (map1 eval1 e1)

  eval2 :: Expr2 -> Int
  eval2 (In2 e2) = phi2 (map2 eval2 e2)
\end{code}

The |Sum| data type is a general purpose data structure that defines
the disjoint union of two other data types.  The union is disjoint
because the constructors |L| and |R| identify the original type any
element of the sum must come from.  (In a real implementation, the
built-in |Either| type would be used rather than writing our own
|Sum|.)

\begin{code}
  data Sum x y = L x | R y deriving (Show,Eq)

  (<+>) :: (x -> z) -> (y -> z) -> (Sum x y -> z)
  l <+> r = \s -> case s of
			 L x -> l x
			 R x -> r x
\end{code}

The type synonym, |E|, represents the sum of the abstract syntax data
types for numbers and addition (|E1|) and division (|E2|).  The |L|
constructor will encapsulate numbers and addition while the |R|
constructor will encapsulate division.  The type |E| is the disjoint
union and may thus be constructed with |L| or |R| and may therefore
be either of the expression types.  Thus, we now have a type that
encapsulates both terms.

\begin{code}
  type E x = Sum (E1 x) (E2 x)
\end{code}

The |Expr| data type represents an individual expression encapsulated
by the |InE| constructor.  This is necessary to identify both |L| and
|R| constructs from |Sum| as expressions.

\begin{code}
  data Expr = InE (E Expr)
\end{code}

With expressions defined, we can now define |map|, |phi| and |eval|
functions using the same technique as for individual terms.  The
composite |map| function, |mapE|, uses |map1| and |map2| to define a
composite map function.  The construction is fascinating and is worth
some investigation.

The signature of |MapE| indicates that we will take a function between
two seemingly arbitrary types, |x| and |y|, and produces a function
that maps the encapsulation of |x| as a term into the encapsulation
of |y| as a term.  This implies that |x| and |y| are not arbitrary at
all.  Because |E| is defined as the |Sum| of |E1| and |E2|, we know
that |x| and |y| must be terms from our abstract syntax.

To form the definition of |MapE|, we simply build a function for
mapping |g| onto each expression type and encapsulate the result
appropriately using the |L| and |R| constructors.  |map1| and |map2|
provide the appropriate mapping functions for each expression type and
we know that expressions of type |E1| should be encapsulated with |L|
and those of type |E2| with |R|.  Thus, it is simple to build a
function for each element of the abstract syntax by composing
encapsulation and map functions.  |L . map1 g| is a function that maps
|g| onto its argument of type |E1| and encapsulates the result with
|L|.  Similarly, |R . map2 g| is a function that operates on
expressions of type |E2|.

Assembling the functions is easily achieved using the |<+>| operation
from |Sum|.  This operation simply examines its argument and
determines whether it is encapsulated with |L| or |R|. If |L|, the
first function argument is selected and applied.  If |R|, the second.
Thus, |mapE| composes |map1| and |map2| to form a mapping function
that works on all term types.

\begin{code}
  mapE :: (x -> y) -> (E x -> E y)
  mapE g = (L . map1 g) <+> (R . map2 g)
\end{code}

Building |PhiE| is analogous to building |mapE|, except that the
domain argument is already encapsulated with |L| or |R|.  This, the
composition operator is used directly to compose |phi1| and |phi2|.

\begin{code}
  phiE :: E Int -> Int
  phiE = phi1 <+> phi2
\end{code}

Building |EvalE| is exactly analogous to building |Eval1| or |Eval2|.
|EvalE| is first mapped onto the expression to evaluate sub-terms and
|phiE| associates the result with its operational semantics.

\begin{code}
  evalE :: Expr -> Int
  evalE (InE e) = phiE (mapE evalE e)
\end{code}

Following are some examples of expressions being evaluated.  These
things aren't trivial to figure out from the paper because there are
no example expressions.  Note that the constructors for the sum type
must be specified along with the actual value.  Further, the
constructor for the outermost expression must also be present as well.
I didn't anticipate this when I went through the examples the first
time.

\begin{code}
  -- Return the value 1
  test0 = evalE (InE (L (Num 1)))

  -- Add 1 and 1
  test1 = evalE (InE (L (Add (InE (L (Num 1))) (InE (L (Num 1))))))

  -- Divide 1 by 1
  test2 = evalE (InE
		 (R (Dvd 
		     (InE (L (Num 1)))
		     (InE (L (Num 1))))))

  -- Divide 1 by 0
  test3 = evalE (InE
		 (R (Dvd
		     (InE (L (Num 1)))
		     (InE (L (Num 0))))))

  -- Divide 1 by 1 and add 1
  test4 = evalE (InE
		 (L (Add
		     (InE (R (Dvd 
			      (InE (L (Num 1)))
			      (InE (L (Num 1))))))
		     (InE (L (Num 1))))))

\end{code}
