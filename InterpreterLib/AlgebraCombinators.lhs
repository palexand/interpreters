\begin{code}
{-# OPTIONS -fallow-overlapping-instances -fallow-undecidable-instances #-}
module AlgebraCombinators where
import Algebras
import SubType
import Functors
import Control.Monad.Reader
\end{code}





\begin{code}
switchAlg f1 f2 alg1 alg2 term = do
    selected <- asks (either (const $ f1 term) (const $ f2 term))
    zipped <- zipFunctor mkSwitch selected term
    asks $ either (const $ apply alg1 zipped ) (const $ apply alg2 zipped)
  where mkSwitch switch comp = local (const switch) comp


data Algebra f alg a => Switch s f alg a = 
     Switch { decorator :: (f a -> f s),
              algFun :: alg a}


instance (MonadReader s m, ZipFunctor f, Algebra f alg (m a)) => 
         Algebra f (Switch s f alg) (m a) where 
  apply alg term = do let selected = decorator alg term
                      term' <- zipFunctor mkSwitch selected term
                      apply (algFun alg) term'
     where mkSwitch switch comp = local (const switch) comp


instance (Algebra f alg a, Algebra f (Switch s f alg) a) =>
         AlgebraBuilder f ((f a -> f s), alg a) (Switch s f alg) a  where
  mkAlgebra (dec,alg) = Switch dec alg
  
\end{code}