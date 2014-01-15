\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}

module InterpreterLib.Terms.ArithTerm where
import InterpreterLib.Algebras
import InterpreterLib.Functors
\end{code}

\begin{code}
data ArithTerm x = Add x x 
                 | Sub x x
                 | Mult x x
                 | Div x x
                 | NumEq x x
                 | Num Int

instance Functor ArithTerm where
  fmap f (Add x y) = Add (f x) (f y)
  fmap f (Sub x y) = Sub (f x) (f y)
  fmap f (Mult x y) = Mult (f x) (f y)
  fmap f (Div x y) = Div (f x) (f y)
  fmap f (NumEq x y) = NumEq (f x) (f y)
  fmap f (Num x)   = Num x

instance ZipFunctor ArithTerm where
  zipFunctor f (Add x y) (Add a b) = return $ Add (f x a) (f y b)
  zipFunctor f (Sub x y) (Sub a b) = return $ Sub (f x a) (f y b)
  zipFunctor f (Mult x y) (Mult a b) = return $ Mult (f x a) (f y b)
  zipFunctor f (Div x y) (Div a b) = return $ Div (f x a) (f y b)
  zipFunctor f (NumEq x y) (NumEq a b) = return $ NumEq (f x a) (f y b)
  zipFunctor f (Num x) (Num y) = return $ Num x
  zipFunctor f _ _ = fail "No match"

\end{code}

\begin{code}
data ArithTermAlgebra a = ArithTermAlgebra { add :: AlgSig ArithTerm a,
                                             sub :: AlgSig ArithTerm a,
                                             mult :: AlgSig ArithTerm a,
                                             divide :: AlgSig ArithTerm a,
                                             numEq :: AlgSig ArithTerm a,
                                             num :: AlgSig ArithTerm a
                                           }

instance Algebra ArithTerm ArithTermAlgebra a where
  apply alg t@(Add _ _) = add alg t
  apply alg t@(Sub _ _) = sub alg t
  apply alg t@(Mult _ _) = mult alg t
  apply alg t@(Div _ _) = divide alg t
  apply alg t@(NumEq _ _) = numEq alg t
  apply alg t@(Num _) = num alg t

instance AlgebraBuilder ArithTerm (ArithTerm a -> a) ArithTermAlgebra a where
  mkAlgebra f = ArithTermAlgebra f f f f f f

\end{code}


\begin{code}
data BinOp = AddOp | SubOp | MultOp | DivOp | NumEqOp 

decodeOp = fst . decode
decodeArgs = snd . decode

decode (Add x y) = (AddOp, (x,y))
decode (Sub x y) = (SubOp, (x,y))
decode (Mult x y) = (MultOp, (x,y))
decode (Div x y) = (DivOp, (x,y))
decode (NumEq x y) = (NumEqOp, (x,y)) 

\end{code}



\begin{code}
mkAdd = mkTerm2 Add
mkSub = mkTerm2 Sub
mkMult = mkTerm2 Mult
mkDiv = mkTerm2 Div
mkNumEq = mkTerm2 NumEq
mkNum = mkTerm Num
\end{code}