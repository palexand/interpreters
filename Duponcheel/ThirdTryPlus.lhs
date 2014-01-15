\section{Third Try Plus Multiplication}

\begin{code}
  module ThirdTryPlus where
\end{code}

The |Rec| type remains unchanged.

\begin{code}
  data Rec f = In (f (Rec f))
\end{code}

Abstract syntax is added for multiplication.

\begin{code}
  data E1 x 
      = Num Int
      | Add x x
	deriving (Eq,Show)

  data E2 x = Dvd x x deriving (Eq,Show)

  data E3 x = Mul x x deriving (Eq,Show)
\end{code}

An expression type is added for multiplication using |Rec|.

\begin{code}
  type Expr1 = Rec E1
  type Expr2 = Rec E2
  type Expr3 = Rec E3
\end{code}

The definitions for semantic functions, |map|, |phi|, and |eval|
functions for each expression do not change.  New instances are added
for multiplication.  These are identical to those added for Second Try
Plus Multiplication.

\begin{code}
  num = id         -- Num1 Int
  add x y = x+y    -- Add1 Expr1 Expr1
  dvd x y = if y==0 then error "Divide by zero" else x `div` y
  mul x y = x*y     -- Define semantics of times

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
  eval1 (In e1) = phi1 (map1 eval1 e1)

  eval2 :: Expr2 -> Int
  eval2 (In e2) = phi2 (map2 eval2 e2)

  eval3 :: Expr3 -> Int
  eval3 (In e3) = phi3 (map3 eval3 e3)
\end{code}

The definition of |Sum| remains unchanged:

\begin{code}
  data Sum x y = L x | R y deriving (Show,Eq)

  (<+>) :: (x -> z) -> (y -> z) -> (Sum x y -> z)
  l <+> r = \s -> case s of
			 L x -> l x
			 R x -> r x
\end{code}

The changes to the composite structure mimic those done to extend try.
Nothing else changes.

\begin{code}
  newtype E x = MkE (Sum (E3 x) (Sum (E1 x) (E2 x)))

  unE  (MkE x) = x

  type Expr = Rec E

  mapE :: (x -> y) -> (E x -> E y)
  mapE g
      = MkE . ((L . map3 g) <+> ((R . L . map1 g) <+> (R . R . map2 g))) . unE

  phiE :: E Int -> Int
  phiE = (phi3 <+> (phi1 <+> phi2)) . unE

  evalE :: Expr -> Int
  evalE (In e) = phiE (mapE evalE e)
\end{code}

Again, some examples to try out.

\begin{code}
  -- Return the value 1
  test0 = evalE (In (MkE (R (L (Num 1)))))

  -- Add 1 and 1
  test1 = evalE (In 
		 (MkE (R (L (Add
			     (In (MkE (R (L (Num 1)))))
			     (In (MkE (R (L (Num 1))))))))))

  -- Divide 1 by 1
  test2 = evalE (In
		 (MkE (R (R (Dvd
			     (In (MkE (R (L (Num 1)))))
			     (In (MkE (R (L (Num 1))))))))))

  test3 = evalE (In (MkE (L (Mul
			     (In (MkE (R (L (Num 2)))))
			     (In (MkE (R (L (Num 2)))))))))

  test4 = evalE (In (MkE (R (R (Dvd
				(In (MkE (R (L (Num 1)))))
				(In (MkE (R (L (Num 1))))))))))
\end{code}

Again, adding some trivial syntax:

\begin{code}
  num1 = (In (MkE (R (L (Num 1)))))
  num2 = (In (MkE (R (L (Num 2)))))
  num3 = (In (MkE (R (L (Num 3)))))
  numx x = (In (MkE (R (L (Num x)))))
  divide x y = (In (MkE (R (R (Dvd x y)))))
  times x y = (In (MkE (L (Mul x y))))
  plus x y = (In (MkE (R (L (Add x y)))))

  test5 = (num2 `times` num3)

  test6 = (num1 `plus` (num2 `divide` num3))

  test7 = (num1 `plus` (num3 `divide` num2))

  test8 = (num2 `times` (numx 100))

  test9 = (num2 `divide` (numx 0))
\end{code}

Once again, we find that extending the interpreter is reasonably easy
by defining a new interpreter for the new abstract syntax constructs.
The |Rec| type simplifies this process somewhat and makes the common
structure of terms clear.  Furthermore, in unequivocally shows that
the expression data types are in fact fixed point data types.

\section{Functors and Algebras}

The structure of |map| and |phi| functions have a common form that can
be abstracted into |class| definitions.  A |Functor| must define a
|map| function that takes a function |f| and maps it over a function
definition.  Definitions for |E1|, |E2|, |E3|, and |E| are all
functors whose |map| functions are already defined as |map1|, |map2|,
|map3|, and |mapE|.

\begin{code}
  class Functor f where
      map :: (x -> y) -> (f x -> f y)

  instance ThirdTryPlus.Functor E1 where
      map = map1

  instance ThirdTryPlus.Functor E2 where
      map = map2

  instance ThirdTryPlus.Functor E3 where
      map = map3

  instance ThirdTryPlus.Functor E where
      map = mapE
\end{code}

The common structure for |phi| functions is abstracted as an |Algebra|
that defines a function, |phi|, that maps a |Functor|, |f|, and
argument of type |a| to a value of type |a|.  The |phi| functions
define algebras for each expression type.

\begin{code}
  class ThirdTryPlus.Functor f => Algebra f a where
      phi :: f a -> a

  instance Algebra E1 Int where
      phi = phi1

  instance Algebra E2 Int where
      phi = phi2

  instance Algebra E3 Int where
      phi = phi3

  instance Algebra E Int where
      phi = phiE
\end{code}

We can now define a general |eval| function for all algebras.  Given
|Algebra f a|, an |eval| function is directly defined as follows:

\begin{code}
  eval :: Algebra f a => Rec f -> a
  eval (In e) = phi (ThirdTryPlus.map eval e)
\end{code}

Unfortunately, the general |eval| function doesn't enable enough type
inference to find a type value for |a|.  Thus, the following |evalInt|
function is generally defined for evaluation functions that return
|Int| types:

\begin{code}
  evalInt :: Algebra f Int => Rec f -> Int
  evalInt (In e) = phi (ThirdTryPlus.map eval e)
\end{code}

