\begin{code}
module Semantics.ULC.Evaluation where

import Algebras
import Functors
import SubType

import Terms.LambdaTerm
import Terms.VarTerm

import Control.Monad.Reader
import Control.Monad.Error
\end{code}

\begin{code}
data FunType a = FunType (a -> a)
\end{code}

\begin{code}
lambda_value (Lam var () body) = returnInj $ FunType fun
  where fun arg = do val <- arg
                     local ((var,return val):) body

lambda_name (Lam var () body) = returnInj $ FunType fun
  where fun arg = local ((var,arg):) body

application (App f arg) = do val <- f
                             (FunType fun) <- checkType val
                             fun arg

variable :: MonadReader [(Name, m a)] m => VarTerm (m a) -> (m a)
variable (VarTerm x) = do v <- asks (lookup x)
                          maybe (fail ("No such variable " ++ x)) id v

\end{code}

\begin{code}

cbv_semantics = (LambdaTermAlgebra { lam = lambda_value,
                                     app = application}) @+@ (VarTermAlgebra variable)
cbn_semantics = (LambdaTermAlgebra { lam = lambda_name,
                                     app = application}) @+@ (VarTermAlgebra variable)
\end{code}


\begin{code}
type Env vtype = [(Name, EMonad vtype vtype)]
newtype EMonad vtype a = EMonad {runEMonad :: (ErrorT String (Reader (Env vtype)) a)}
instance Monad (EMonad vtype) where
  return = EMonad . return
  (EMonad m) >>= f = EMonad  $ do v <- m
                                  runEMonad (f v)

instance MonadError String (EMonad vtype) where
  throwError = EMonad . throwError
  (EMonad m) `catchError` handler = EMonad $ m `catchError` (runEMonad . handler)

instance MonadReader (Env vtype) (EMonad vtype) where
  ask = EMonad ask
  local f (EMonad m) = EMonad (local f m)


evaluate sem term = (either error id (runReader (runErrorT (runEMonad (cata sem term))) [])) :: VSpace

newtype VSpace = VSpace (FunType (EMonad VSpace VSpace))

instance SubType (FunType (EMonad VSpace VSpace)) VSpace where
  inj = VSpace
  prj (VSpace f) = Just f

instance Show VSpace where
  show (VSpace (FunType f)) = "function"
\end{code}