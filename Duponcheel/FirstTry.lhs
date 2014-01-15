\section{First Try}

Module |FirstTry| presents a Haskell encoding of Duponcheel's First
Try at building a modular interpreter:

\begin{code}
  module FirstTry where
\end{code}

The interpreter defined is a simple implementation of a language that
supports adding and dividing integer numbers.  The approach is to
first define an interpreter for numbers and addition (|eval1|), then
extend the interpreter to include division (|eval2|).

First, we define data structures for |Expr1| representing numbers and
addition:

\begin{code}
  data Expr1
      = Num1 Int
      | Add1 Expr1 Expr1
	deriving (Eq,Show)
\end{code}

Next, define semantic functions associated with the two operations.
|num| simply returns the number encapsulated buy |Num| while |add|
sums the numbers associated with an |Add| term.

\begin{code}
  num = id         -- Num1 Int
  add x y = x+y    -- Add1 Expr1 Expr1
\end{code}

The |eval1| function associates semantics with abstract syntax.  For
each term defined in |Expr1|, |eval1| generates a semantic
interpretation:

\begin{code}
  eval1 :: Expr1 -> Int 
  eval1 (Num1 x) = num x                -- Num1 x
  eval1 (Add1 x y) = eval1 x `add` eval1 y  -- Add1 x y
\end{code}

We now have an interpreter defined for numbers and addition operations
where both are defined as constructors in a single data type.  What we
are attempting to demonstrate is that such interpreters can be
extended by adding further operations and types.  In this case, we'll
add a division operation to the initial interpreter.

Because the initial interpreter is not particularly modular, the
extension requires us to touch almost all of the original code.
First, the |Expr1| data type is modified to include a division
operation and becomes |Expr|:

\begin{code}
  data Expr
      = Num Int
      | Add Expr Expr
      | Dvd Expr Expr
	deriving (Show,Eq)
\end{code}

We can reuse the semantic definitions for |Num| and |Add|, but we need
a semantic definition for |Dvd|:

\begin{code}
  dvd x y = if y==0 then error "Divide by zero" else x `div` y
\end{code}

The new |eval| function extends |eval1| by adding a case for the
division operation.  Note that the function must be rewritten and
cannot simply be extended.

\begin{code}
  eval :: Expr -> Int
  eval (Num x) = id x
  eval (Add x y) = eval x `add` eval y
  eval (Dvd x y) = eval x `dvd` eval y
\end{code}

With this, we have an interpreter for an expression language that
allows numbers, addition and division.  Each new type or operation
requires the same effort of rewriting expression syntax, adding
semantics and modifying the evaluation function.  With the exception
of the semantic functions, virtually none of the existing code goes
unmodified.  For small interpreters, this is not a problem.  For
larger, more complex interpreters extension becomes more difficult.

