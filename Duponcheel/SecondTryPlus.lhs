\section{Second Try Plus Multiplication}

So what's the point?  Duponcheel has taken a pretty simple interpreter
and obfuscated it to give us an interpreter that uses what will become
fixed point types.  To demonstrate the point, we'll extend the Second
Try interpreter to include a new term that performs multiplication.
Although all the code is repeated here, it is surprising how little is
modified.

\begin{code}
  module SecondTryPlus where
\end{code}

The definitions for |E1| and |E2| are unchanged.  A new, separate
definition is added for |E3| to represent multiplication terms:

\begin{code}
  data E1 x
      = Num Int
      | Add x x
	deriving (Eq,Show)

  data E2 x = Dvd x x deriving (Eq,Show)

  data E3 x = Mul x x deriving (Eq,Show)
\end{code}

The definitions for |Expr1| and |Expr2| are unchanged.  A new
definition for |Expr3| representing multiplication is added.

\begin{code}
  data Expr1 = In1 (E1 Expr1) deriving (Eq,Show)

  data Expr2 = In2 (E2 Expr2) deriving (Eq,Show)

  data Expr3 = In3 (E3 Expr3) deriving (Eq,Show)
\end{code}

Semantic functions also remain unchanged.  A function for
multiplication is added:

\begin{code}
  num = id          -- Define semantics of num
  add x y = x+y     -- Define semantics of add
  dvd x y = if y==0 -- Define semantics of divide
	    then error "Divide by zero"
	    else x `div` y
  mul x y = x*y     -- Define semantics of times
\end{code}

The original |map| and |phi| functions again remain unmodified.
|map3| and |phi3| are added for multiplication operations.  |eval3| is
also added to show that we can build an interpreter separately for
multiplication.

\begin{code}
  map1 :: (x->y) -> (E1 x -> E1 y)
  map1 g (Num n) = Num n
  map1 g (Add e f) = (Add (g e) (g f))

  map2 :: (x->y) -> (E2 x -> E2 y)
  map2 g (Dvd e f) = (Dvd (g e) (g f))

  map3 :: (x->y) -> (E3 x -> E3 y)
  map3 g (Mul e f) = (Mul (g e) (g f))

  phi1 :: E1 Int -> Int
  phi1 (Num n) = num n
  phi1 (Add e f) = e `add` f

  phi2 :: E2 Int -> Int
  phi2 (Dvd e f) = e `dvd` f

  phi3 :: E3 Int -> Int
  phi3 (Mul e f) = e `mul` f

  eval1 :: Expr1 -> Int
  eval1 (In1 e1) = phi1 (map1 eval1 e1)

  eval2 :: Expr2 -> Int
  eval2 (In2 e2) = phi2 (map2 eval2 e2)

  eval3 :: Expr3 -> Int
  eval3 (In3 e3) = phi3 (map3 eval3 e3)
\end{code}

The definition for sum does not change.

\begin{code}
  data Sum x y = L x | R y deriving (Show,Eq)

  (<+>) :: (x -> z) -> (y -> z) -> (Sum x y -> z)
  l <+> r = \s -> case s of
			 L x -> l x
			 R x -> r x
\end{code}

All we have done to this point is define an interpreter for
multiplication operations by defining |map|, |phi| and |eval| for the
new abstract syntax.  None of the previous definitions have been
modified in any way.  We will now integrate the multiplication syntax
into the abstract syntax for the entire language and integrate the
interpreters.

First, define a type shorthand that adds E3 to the expression.
Effectively, the abstract syntax for expressions becomes:

\begin{alltt}
  (Sum multiplication (Sum division addition)) 
\end{alltt}

The |Expr| data structure does not change.

\begin{code}
  type E x = Sum (E3 x) (Sum (E1 x) (E2 x))

  data Expr = InE (E Expr)
\end{code}

|mapE| uses the sum composition operator to map an operation across
the entire expression abstract syntax.  The signature does not change.
The function body rebuilds the expression around the application of
map to each possible expression type.  Note the use of |R| and |L| to
build what is in effect a tree containing possible expressions.  Any
number of expressions can be added in this way.  |phiE| is similarly
modified and |evalE| remains unchanged.

\begin{code}
  mapE :: (x -> y) -> (E x -> E y)
  mapE g = (L . map3 g) <+> ((R . L . map1 g) <+> (R . R . map2 g))

  phiE :: E Int -> Int
  phiE = phi3 <+> (phi1 <+> phi2)

  evalE :: Expr -> Int
  evalE (InE e) = phiE (mapE evalE e)
\end{code}

Following are more examples the integrate multiplication.  Note how
|R| and |L| must be used together to construct some terms.  This is
painful without a concrete syntax to compile from, but simple when
writing compilers that automatically generate structures.

\begin{code}
  -- Return the value 1
  test0 = evalE (InE (R (L (Num 1))))

  -- Add 1 and 1
  test1 = evalE (InE (R (L (Add (InE (R (L (Num 1)))) (InE (R (L (Num 1))))))))

  -- Divide 1 by 1
  test2 = evalE (InE (R (R (Dvd (InE (R (L (Num 1)))) (InE (R (L (Num 1))))))))

  test3 = evalE (InE (L (Mul (InE (R (L (Num 2)))) (InE (R (L (Num 2)))))))

  test4 = evalE (InE (R (R (Dvd (InE (R (L (Num 1)))) (InE (R (L (Num 1))))))))
\end{code}

Now that we've done it the hard way, let's make things easier by
defining names for operators and operands.  Define some useful
variables and functions that provide a kind of ad hoc syntax that we'll use
to define expressions.

\begin{code}
  num1 = (InE (R (L (Num 1))))
  num2 = (InE (R (L (Num 2))))
  num3 = (InE (R (L (Num 3))))
  numx x = (InE (R (L (Num x))))
  divide x y = (InE (R (R (Dvd x y))))
  times x y = (InE (L (Mul x y)))
  plus x y = (InE (R (L (Add x y))))
\end{code}

We can now write expressions that look much more like we think they
should.  Use evalE to evaluate the following examples:

\begin{code} 
  test5 = (num2 `times` num3)

  test6 = (num1 `plus` (num2 `divide` num3))

  test7 = (num1 `plus` (num3 `divide` num2))

  test8 = (num2 `times` (numx 100))

  test9 = (num2 `divide` (numx 0))
\end{code}

Any reasonable assessment of this interpreter reveals that to add a
new term type to the interpreter, we simply need to define an
interpreter for that term type and compose it with those that already
exist.  Composing the new evaluation function is quite systematic and
achieved using features of the |Sum| type constructor.  No
modification is required for existing interpreter modules.  In
addition to dramatically simplifying interpreter construction,
debugging becomes much simpler as the new interpreter can be tested
and debugged in a modular fashion.
