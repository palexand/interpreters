\section{Third Try}

The Third Try from Duponcheel attempts to abstract common structures
from individual interpreters and syntax into a common form.  The
result is a parameterized data structure that forms fixed point forms
in a common, consistent manner.

\begin{code}
  module ThirdTry where
\end{code}

All the |Expr| types from |SecondTry| share a common form that can be
abstracted and parameterized as |Rec|:

\begin{code}
  data Rec f = In (f (Rec f))
\end{code}

The |Rec| type creates a fixed point data type from some element |f|.
Note the constructor |In| that accomplishes the same task as |In1|,
|In2|, and |InE| from previous examples.

We will need to define abstract syntax for each expression type.
These definitions do not change from earlier examples, they will
simply be packaged differently for the interpreter.

\begin{code}
  data E1 x 
      = Num Int
      | Add x x
	deriving (Eq,Show)

  data E2 x = Dvd x x deriving (Eq,Show)
\end{code}

Now use |Rec| to define fixed point types for |Expr1| and |Expr2|.

\begin{code}
  type Expr1 = Rec E1
  type Expr2 = Rec E2
\end{code}

The definitions for semantic functions, |map|, |phi|, and |eval|
functions for each expression do not change.

\begin{code}
  num = id         -- Num1 Int
  add x y = x+y    -- Add1 Expr1 Expr1
  dvd x y = if y==0 then error "Divide by zero" else x `div` y

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

  eval1 :: Expr1 -> Int
  eval1 (In e1) = phi1 (map1 eval1 e1)

  eval2 :: Expr2 -> Int
  eval2 (In e2) = phi2 (map2 eval2 e2)
\end{code}

The definition of |Sum| remains unchanged:

\begin{code}
  data Sum x y = L x | R y deriving (Show,Eq)

  (<+>) :: (x -> z) -> (y -> z) -> (Sum x y -> z)
  l <+> r = \s -> case s of
			 L x -> l x
			 R x -> r x
\end{code}

Now the fun begins.  An expression in our language is the sum of the
expression for addition and division.  Thus, we use the |Sum| type to
do our composition.  In Gopher, the |type| constructor behaves
differently than the |type| synonym definition operator in Haskell.
Thus, the definition is altered to use |newtype| requiring the
insertion of a new type constructor, |MkE|.  In addition, an
un-constructor is defined to pull data out of |MkE|.

\begin{code}
  newtype E x = MkE (Sum (E1 x) (E2 x))

  unE  (MkE x) = x
\end{code}

The composite |Expr| definition is the fixed point type created from
|E| that we just defined.

\begin{code}
  type Expr = Rec E
\end{code}

The map function for the composite expression, |E|, is basically the
same function with the addition of functions to accommodate the |MkE|
constructor we had to introduce earlier. Specifically, |mapE| must
first extract the expression from the |MkE| constructor because the
individual map functions aren't aware of the need to package the
expression.  Once processed, the expression is repackaged.  Similarly,
|phiE| must un-package its argument.  Because it is making a semantic
mapping, its result should not be repackaged.  |evalE| does not
change.

\begin{code}
  mapE :: (x -> y) -> (E x -> E y)
  mapE g = MkE . ((L . map1 g)  <+> (R . map2 g)) . unE

  phiE :: E Int -> Int
  phiE = (phi1 <+> phi2) . unE

  evalE :: Expr -> Int
  evalE (In e) = phiE (mapE evalE e)
\end{code}

Again, some examples to try out.

\begin{code}
  -- Return the value 1
  test0 = evalE (In (MkE (L (Num 1))))

  -- Add 1 and 1
  test1 = evalE (In (MkE 
		     (L (Add
			 (In (MkE (L (Num 1)))) 
			 (In (MkE (L (Num 1))))))))

  -- Divide 1 by 1
  test2 = evalE (In (MkE
		     (R (Dvd
			 (In (MkE (L (Num 1))))
			 (In (MkE (L (Num 1))))))))

\end{code}

