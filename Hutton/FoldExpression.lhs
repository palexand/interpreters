\section{Fold For Expressions}

Module |FoldForExpressions| presents a Haskell encoding of
Hutton's interpreter and definition of fold over expressions:

\begin{code}
  module FoldForExpressions where
\end{code}

First, we define data structures for |Expr| representing numbers and
addition.  This is a parameterized version of the expression data type
defined in the Duponcheel interpreters:

\begin{code}
  data (Show a,Eq a) => Expr a
      = Val a
      | Add (Expr a) (Expr a)
	deriving (Eq,Show)
\end{code}

It's easy now to define direct evaluation of the expression data
structure using direct recursion.  Note that |Expr| is instantiated
over |Int| in this evaluator:

\begin{code}
  eval1 :: Expr Int -> Int
  eval1 (Val n) = n
  eval1 (Add x y) = eval1 x + eval1 y
\end{code}

The function |phi| is actually defined as |deno| in the Hutton paper.
I have used |phi| to be consistent with Duponcheel.  The functions
|f| and |g| provide the semantics for |Val| and |Add|.

\begin{code}
  f = id
  g = (+)

  phi (Val n) = f n
  phi (Add x y) = g (phi x) (phi y)
\end{code}

The |fold| operation defines a general fold function over expressions.
The signature of the fold is interesting.  So much so that I let
\texttt{Haskell}'s type inference system find it for me.  |a| reflects
the type of the value encapsulated by |Val a| while |b| reflects the
domain of what will be the evaluation function.  Specifically, |a->b|
defines the signature of the value extraction function.  For |eval2|,
that function is |id| because we are simply extracting the value.  For
|comp| later, it will be |Inst|, the instruction generated for the
stack machine.

\begin{code}
  fold :: (Eq a,Show a) => (a -> b) -> (b -> b -> b) -> Expr a -> b
  fold f g (Val n) = f n
  fold f g (Add x y) = g (fold f g x) (fold f g y)
\end{code}

The evaluation function, |eval1|, defined earlier can now be redefined
using |fold| and specifying the semantic mappings for |Val| and |Add|
respectively.

\begin{code}
  eval2 = fold id (+)
\end{code}

Hutton also defines a compiler function, |comp|, that generates a
sequence of instructions for a stack machine.  |Inst| is a data type
representing possible instructions for the machine.  

\begin{code}
  data Inst = PUSH Int | ADD deriving (Eq,Show)
\end{code}

|comp| translates an expression defined over integers into a list of
instructions.  When looking at the |fold|, |a| is instantiated with
|Expr Int| while |b| is instantiated with |[Inst]|.  Initially, I was
confused with the definition of |g|.  The fold will actually evaluate
the arguments to |Add| and return them.  It thus makes sense that |g|
simply concatenates the lists of instructions and tacks an |ADD| onto
the end.

\begin{code}  
  comp :: Expr Int -> [Inst]
  comp = fold f g
	 where
	   f n = [PUSH n]
	   g xs ys = xs ++ ys ++ [ADD]
\end{code}

Hutton extends |eval2| to include variables and a program store.  I'm
going to extend |eval2| like I did for Duponcheel by adding a multiply
operation.  It's not particularly difficult, however it becomes clear
that the interpreter is not particularly modular or extensible.

First, we need to extend the expression data structure to include the multiplication operation:

\begin{code}
  data (Show a,Eq a) => Expr1 a
      = Val1 a
      | Add1 (Expr1 a) (Expr1 a)
      | Mul1 (Expr1 a) (Expr1 a)
	deriving (Eq,Show)
\end{code}

Then we have to redefine |fold| as |fold1| to take three functions to
include the semantics for multiply:

\begin{code}
  fold1 :: (Show a,Eq a) => (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr1 a -> b
  fold1 f g h (Val1 x) = f x
  fold1 f g h (Add1 x y) = g (fold1 f g h x) (fold1 f g h y)
  fold1 f g h (Mul1 x y) = h (fold1 f g h x) (fold1 f g h y)
\end{code}

Now we can define |eval3| to perform the appropriate evaluation:

\begin{code}
  eval3 = fold1 id (+) (*)
\end{code}

Hutton continues to generize the evaluation function by adding a
|Store| and variable access to the interpetation.  He does not provide
a full definition for |Store|, so we'll try to here.  First we define
a new |Expr| type that includes the concept of a variable:

\begin{code}
  data Expr2 ty
      = Val2 ty
      | Add2 (Expr2 ty) (Expr2 ty)
      | Var2 String
\end{code}

Note that the |Expr2| type is parameterized over the contained data,
|n|, and the variable name type, |v|.  This will allow the most
general possible |Store| to be defined.

Next, we define a new |fold| that includes the processing of variable
references.  |h| now provides a lookup capability for variables in the
store.

\begin{code}
  fold2 f g h (Val2 x) = f x
  fold2 f g h (Add2 x y) = g (fold2 f g h x) (fold2 f g h y)
  fold2 f g h (Var2 c) = h c
\end{code}

We still need a |Store| and |find| function for records in the store.
The easiest |Store| to define is a list of pairs whose first element
is a |String| and whose second is a value:

\begin{code}
  data Binding n = Binding String n

  type Store = [(Binding Int)]

  find :: String -> Store -> Int
  find v [] = error ("Variable " ++ v ++ " not found in store")
  find n1 ((Binding n2 z):bs) = if (n1==n2) then z else find n1 bs
\end{code}

Now we can write the |eval| function over this new data structure with
an associated store like Hutton:

\begin{code}
  evalS :: (Expr2 Int) -> (Store -> Int)
  evalS = fold2 f g h
          where
	     f n = \s -> n
	     g fx fy = \s -> fx s + fy s
	     h n = \s -> find n s
\end{code}

This function maps an expression to a function mapping |Store| to
|Int| rather than just an |Int|.  We must in effect provide a |Store|
to perform a full evaluation.  However, that |Store| is not mutable -
there is no mechanism for adding to or deleting from the |Store|.

Some interesting examples:

\begin{code}
  testS = (Add2 (Var2 "Test") (Var2 "x"))
  failS = (Add2 (Var2 "test") (Var2 "x"))
  storeS :: Store = [(Binding "Test" 3),(Binding "x" 4)]
\end{code}

You can now enter the following at the command line to see what's going on:

\begin{spec}
  evalS storeS testS

  evalS storeS failS
\end{spec}

This is interesting from a semantics definition perspective.  However,
it is not a particularly useful way to define modular interpreters as
Duponcheel does.

One question is whether the |Sum| type used buy Duponcheel to form
composite data types and composite application functions.  Hutton's
approach uses the position of the folded function in the argument list
to determine which function to apply.  Duponcheel defines a function
composition operation over the |Sum| type to accomplish a similar task.

First, define new data types for each element of the expression:

\begin{spec}
  data Expr2 a = Val2 a
  data Expr3 a = Add2 a a
\end{spec}

Now reproduce the |Sum| definition from Duponcheel:

\begin{spec}
  data Sum a b
      = L a
        R b

  (<+>) :: (x -> z) -> (y -> z) -> ((Sum x y) -> z)
  f <+> g = s -> case s of
			 (L x) -> f x
			 (R x) -> g x
\end{spec}

Now define a |Term| type to represent both possible term elements:

\begin{spec}
  type Term = Sum (Expr2 Int) (Expr3 Int)
\end{spec}

Finally, define a mapping function from the composite of the |id| and
|(+)| operations:

\begin{spec}
  j = id <+> (+)
\end{spec}

If all goes well, we should be able to fold |j| through the structure.

\begin{spec}
  fold2 j (L (Val2 x)) = j x
  fold2 j (R (Add2 x y)) = j (fold2 j x) (fold2 j y)
\end{spec} 

But all doesn't go well.  The problem is that the arity of |j| is
fixed.  Thus, there is no way to apply it to both |x| in the first
case and the results of |fold2| applied twice in the second case.
What's happening here is actually pretty simple.  Hutton's fold
semantics unpackages the arguments to |Add2| before applying |Fold2|.
This explains why: (i) the signature for |g| above maps two elements
of the type \emph{encapsulated by} |Val| to an element of the same
type rather than two elements of the |Expr|; and (ii) why the function
composition cannot be applied in the same way.  It would be
interesting to attempt to rewrite the fold to achieve this end.
