-- Implements a simple language embedded in the reactive resumption
-- monad, ReactT.  First useful application example of resumptions.

{-# OPTIONS -fglasgow-exts -fno-monomorphism-restriction #-}
import InterpreterLib.Algebras
import InterpreterLib.Functors
import InterpreterLib.Terms.ArithTerm
import InterpreterLib.Terms.IfTerm
import InterpreterLib.Terms.LambdaTerm
import InterpreterLib.Terms.VarTerm
import InterpreterLib.Terms.ImperativeTerm
import Data.Ratio
import Monad
import Control.Monad.Reader
import Control.Monad.State
import Monad.Resumption

-- Basic value type including numbers, booleans and functions
data Value 
    = ValNum Int
    | ValBool Bool
    | ValLambda (ValueMonad -> ValueMonad)
    | ValRef Int
    | ValNull
      
instance Show Value where
    show (ValNum x) = show x
    show (ValBool x) = show x
    show (ValLambda x) = show x
    show (ValRef x) = "<Reference " ++ (show x) ++ ">"
    show ValNull = "null"

instance Eq Value where
    (==) (ValNum x) (ValNum y) = x == y
    (==) (ValBool x) (ValBool y) = x == y
    (==) (ValLambda x) (ValLambda y) = x == y
    (==) (ValRef x) (ValRef y) = x == y
    (==) ValNull ValNull = True

-- Variable environment.  String used to reference Value associated with
-- variable of that name.
type Gamma = [(String,Value)]

-- Reference environment.  Integer used to reference Value associated with
-- location of that number
type Env = [Value]

-- Variable utility functions
addSubst v g = v:g

findSubstValue s g = case (lookup s g) of
                                       (Just x) -> x
                                       Nothing -> error "Variable not in scope"

-- Reference utility functions
addRef t st = t:st

deref (ValRef x) refs = refs!!x

replace x v vs = (take x vs) ++ (v:(drop (x+1) vs))

-- Value type nesting monads

data SignalVal = Cont | Ack

instance Signal SignalVal where
    cont = Cont
    ack = Ack

type ValueMonad = 
    (ReactT SignalVal SignalVal (StateT Env (Reader Gamma))) Value

instance Show (ValueMonad -> ValueMonad) where
    show f = "<Function Value>"

instance Eq (ValueMonad -> ValueMonad) where
    (==) _ _ = error "Cannot compare function values."


-- Define numeric properties over value so we can use +,-,*,/ and friends
-- rather than define a call helper functions everywhere.  This is a big
-- enough pain that helpers might be preferable.
instance Num Value where
    (+) (ValNum x) (ValNum y) = ValNum (x+y)
    (+) (ValBool x) (ValBool y) = ValBool (x || y)
    (-) (ValNum x) (ValNum y) = ValNum (x-y)
    (*) (ValNum x) (ValNum y) = ValNum (x*y)
    (*) (ValBool x) (ValBool y) = ValBool (x && y)
    negate (ValNum x) = ValNum (-x)
    negate (ValBool x) = ValBool (not x)
    abs (ValNum x) = ValNum (abs x)
    signum (ValNum x) = ValNum (signum x)
    fromInteger = ValNum . fromInteger

instance Enum Value where
    succ (ValNum x) = ValNum (x+1)
    pred (ValNum x) = ValNum (x-1)
    toEnum = ValNum
    fromEnum (ValNum x) = x

instance Integral Value where
    (div) (ValNum x) (ValNum y) = ValNum (div x y)
    quotRem (ValNum x) (ValNum y) = case (quotRem x y) of
                                      (x,y) -> (ValNum x, ValNum y)
    toInteger (ValNum x) = toInteger x

instance Real Value where
    toRational (ValNum x) = (toInteger x) % (toInteger 1)

instance Ord Value where
    (<) (ValNum x) (ValNum y) = x < y
    (>=) (ValNum x) (ValNum y) = x >= y
    (>) (ValNum x) (ValNum y) = x > y
    (<=) (ValNum x) (ValNum y) = x <= y
    max (ValNum x) (ValNum y) = ValNum (max x y)
    min (ValNum x) (ValNum y) = ValNum (min x y)

-- Now define the interpreter.  First, define the functions for the
-- semantices of various operations.

phiArith (Add x1 x2) = liftM2 (+) x1 x2
phiArith (Sub x1 x2) = liftM2 (-) x1 x2
phiArith (Mult x1 x2) = liftM2 (*) x1 x2
phiArith (Div x1 x2) = liftM2 (div) x1 x2
phiArith (NumEq x1 x2) = liftM2 (\v1 -> \v2 -> if (v1==v2) then 1 else 0) x1 x2
phiArith (Num x1) = return (ValNum x1)

phiIf (IfTerm x1 x2 x3) = do { (ValBool x1') <- x1
                             ; liftM id (if x1' then x2 else x3)
                             }
phiIf TrueTerm = return $ ValBool True
phiIf FalseTerm = return $ ValBool False

-- Need to undo the value addition.
phiLambda (Lam s _ t)
    = do { g <- ask
         ; return $ ValLambda (\v -> do { v' <- v
                                        ; local (const ((s,v'):g)) t
                                        })
         }

phiLambda (App t1 t2)
    = do { (ValLambda f) <- t1
         ; (f t2)
         }

phiVar :: (VarTerm ValueMonad) -> ValueMonad
phiVar (VarTerm s)
    = do { v <- asks (lookup s)
         ; case v of
                  (Just x) -> return x
                  Nothing -> error "Variable not found"
         }
-- The dummy term definition helps Haskell figure out what type VarTerm
-- is defined over.  It is never used.
phiVar (DummyTerm x)
    = return (ValBool True)

phiImp (NewRef t)
    = do { st <- get
         ; t' <- t
         ; put (addRef t' st)
         ; return $ (ValRef (length st))
         }
phiImp (DeRef t)
    = do { t' <- t
         ; st <- get
         ; return $ deref t' st
         }
phiImp (Assign t1 t2)
    = do { t1' <- t1
         ; t2' <- t2
         ; case t1' of
                    (ValRef r) -> do { env <- get
                                     ; put (replace r t2' env)
                                     ; return ValNull
                                     }
                    _ -> error "Cannot assign to non-reference value"
         }
phiImp (SeqTerm t1 t2)
    = do { t1' <- t1
         ; t2' <- t2
         ; return $ t2'
         }


-- Build the term type and the term language.

type TermType
    = ArithTerm
    :$: IfTerm
    :$: LambdaTerm ()
    :$: VarTerm
    :$: ImperativeTerm

type TermLang = Fix TermType

-- Build the explicit algebra.

termAlgebra
    = (mkAlgebra phiArith)
    @+@ (mkAlgebra phiIf)
    @+@ (mkAlgebra phiLambda)
    @+@ (mkAlgebra phiVar)
    @+@ (mkAlgebra phiImp)

-- Write a basic handler and scheduler function and type class.
-- scheduler selects the monad to be resumed.  handler resumes it and
-- generates the new set of resumptions.

class (MonadReact m req rsp) => Simulator m req rsp a where
    scheduler :: [(m req rsp a)] -> (m req rsp a)
    handler :: [(m req rsp a)] -> (m req resp a) -> [(m req rsp a)]

instance Simulator (ReactT SignalVal SignalVal) where
    scheduler [] = error "No monad to resume."
    scheduler [rm:rms] = rm
    handler (D v) = return v
    handler (P q r) = [(r 1)]

-- Here are several test terms and an evaluation function to play with.  To
-- evaluate a term, use the following:

--  runReader (runStateT (runXXX (eval termX)) []) []

-- |(eval termX)| returns the monad resulting from evaluating |termX|.
-- |runStateT| then evaluates that monad with the empty environment []|.
-- |runReader| then evaluates the previous monad with the empty gamma.
-- |In the past, we have encapsulated the environment in some kind of
-- |constructed type.  Here, we are just using a list.

-- Shorthand eval function
eval = (cata termAlgebra)

-- Shorthans for (S (Right x)) and (S (Right y))
sright = S . Right

sleft = S . Left

-- Some simple arithmetic terms
term1 = (inn (sleft (Num 1)))
         
one = term1
two = (inn (sleft (Num 2)))
three = (inn (sleft (Num 3)))

term2 = (inn (sleft (Add term1 term1)))

term3 = (inn (sleft (Mult term2 term2)))

term4 = (inn (sleft (Sub term2 term1)))

-- Shorthands for true and false values
termTrue = (inn (sright (sleft TrueTerm)))

termFalse = (inn (sright (sleft FalseTerm)))

-- if term that returns true case
term5 = (inn (sright (sleft (IfTerm termTrue term4 term3))))

-- if term that returns false case
term6 = (inn (sright (sleft (IfTerm termFalse term4 term3))))

-- Constant function that always returns term6
term7 = (inn (sright (sright (sleft (Lam "x" () term6)))))

-- Constant function applied to 1
term8 = (inn (sright (sright (sleft (App term7 term1)))))

-- Variable x
term9 = (inn (sright (sright (sright (sleft (VarTerm "x"))))))

-- Identity function
term10 = (inn (sright (sright (sleft (Lam "x" () term9)))))

-- Identity function applied to 1
term11 = (inn (sright (sright (sleft (App term10 term1)))))

-- NewRef and return
term12 = (inn (sright (sright (sright (sright (NewRef term1))))))

-- Access allocated location
term13 = (inn (sright (sright (sright (sright (DeRef term12))))))

-- Replace allocated location
term14 = (inn (sright (sright (sright (sright (Assign term12 term1))))))

-- -- Lambda accessing memory
-- Create a reference to value 1
term15a = (inn (sright (sright (sright (sright (NewRef one))))))
-- Dereference a reference to value 1
term15b = (inn (sright (sright (sright (sright (DeRef term9))))))
-- Create a lambda dereferencing a reference to value 1
term15 = (inn (sright (sright (sleft (Lam "x" () term15b)))))

-- Applying memory access lambda to location
term16 = (inn (sright (sright (sleft (App term15 term12)))))

-- -- Sequencing two terms
-- Assign 2 to the variable "x"
term17a = (inn (sright (sright (sright (sright (Assign term9 two))))))
-- Sequence assinging 2 to "x" and dereferncing "x"
term17b = (inn (sright (sright (sright (sright (SeqTerm term17a term15b))))))
-- Put the sequence in a lambda with "x" as the variable name
term17c = (inn (sright (sright (sleft (Lam "x" () term17b)))))
-- Evaluate the lambda on a reference to the value 1.  Should return 2
term17 = (inn (sright (sright (sleft (App term17c term15a)))))
