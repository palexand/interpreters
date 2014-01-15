\begin{code}
module Semantics.ULC.Syntactic where

import Algebras
import Functors
import SubType

import Terms.LambdaTerm hiding (mkApp)
import Terms.VarTerm

import Control.Monad.Reader
import Control.Monad.Error

\end{code}


\begin{code}
type Term ty = Fix (Sum (LambdaTerm ty) VarTerm)
instance Show ty => Show (Term ty) where
  show (In (S (Left f))) = show f
  show (In (S (Right g))) = show g


instance Show (VarTerm a) where
  show (VarTerm x) = x


instance (Show ty, Show f) => (Show (LambdaTerm ty f)) where
  show (Lam n ty body) = "\\" ++ n ++ "::" ++ (show ty) ++ " -> " ++ (show body)
  show (App x y) = "(" ++ (show x) ++ ")" ++ "(" ++ (show y) ++ ")"


mkApp :: (Term ()) -> (Term ()) -> (LambdaTerm () (Term ()))
mkApp = App
\end{code}


\begin{code}

data Tm = Lambda Name Tm
        | Application Tm Tm
        | Variable Name deriving Show

eval (Lambda n tm) rho = (Lambda n tm)
eval (Application (Lambda n body) arg) rho = eval body ((n,arg):rho)
eval (Application f arg) rho = eval (Application (eval f rho) arg) rho
eval (Variable n) rho = maybe (error "No Such") id (lookup n rho)

eval1 (Lambda n tm)  = \rho -> (Lambda n tm)
eval1 (Application (Lambda n body) arg) = \rho -> eval1 body ((n,arg):rho)
eval1 (Application f arg) = \rho -> eval (Application (eval1 f rho) arg) rho
eval1 (Variable n) = \rho -> maybe (error "No Such") id (lookup n rho)

eval2 :: Tm -> Reader [(Name,Tm)] Tm
eval2 (Lambda n tm)  = return (Lambda n tm)
eval2 (Application (Lambda n body) arg) =  local ((n,arg):) (eval2 body)
eval2 (Application f arg) =  do f' <- eval2 f
                                eval2 (Application f' arg)
eval2 (Variable n) =   asks (\rho -> maybe (error "No Such") id (lookup n rho))


eval3 :: Tm -> Reader [(Name,Tm)] Tm
eval3 (Lambda n tm)  = return (Lambda n tm)
eval3 (Application f arg) =  do (Lambda n body) <- eval3 f
                                local ((n,arg):) (eval3 body)
eval3 (Variable n) =   asks (\rho -> maybe (error "No Such") id (lookup n rho))

newtype Env vspace = Env [(Name, Reader (Env vspace) vspace)] 
-- eval4 :: Tm -> Reader Env (Reader Env Tm)

data VSpace = VLambda Name (Reader (Env VSpace) VSpace)
            | TermSpace Tm


instance Show VSpace where
  show = show . foo
    where foo (VLambda n body) = let body' = (runReader body (Env [(n,return $ TermSpace $ Variable n)]))
                                 in (Lambda n (foo body'))
          foo (TermSpace t) = t

eval4 (Lambda n tm)  = let tm' = eval4 tm
                       in return $ VLambda n tm'

eval4 (Application f arg) = let f' = eval4 f
                                arg' = eval4 arg
                            in do (VLambda n body) <- f'
                                  local (extendEnv n arg') body

eval4 (Variable n) =  do m <- asks (lookupEnv n)
                         m



lookupEnv n (Env rho) = maybe (error "No Such") id (lookup n rho)
extendEnv n v (Env rho) = Env ((n,v):rho)

lambda_sem t@(Lam n () val) = return (n,val)
app_value (App x y) = do xv <- x
                         t@(Lam n () body) <- checkTerm (out xv)
                         return $ toS t
{-
                         yv <- y
                         local ((n,yv):) body
-}

app_name (App x y) = do xv <- x
                        (Lam n _ body) <- checkType xv
                        local ((n,y):) body

checkTerm tm = case prjF tm of
                 Just t -> return t
                 Nothing -> fail "Incorrect type"


\end{code}


