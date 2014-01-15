\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Resumption where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer

--- Basic resumption monad defining pause and done
data (Monad m) => ResT m a 
    = Done a | Pause (m (ResT m a))

runResT (Done x) = return x
runResT (Pause m) = do x <- m 
                       runResT x


instance (Monad m) => Monad (ResT m) where
    return v = Done v
    (Done v) >>= f = f v
    (Pause m) >>= f = Pause (m >>= \r -> return (r >>= f))

class (MonadResume m n) where
    step :: n a -> m n a

instance (Monad n) => MonadResume ResT n where
    step x = Pause (x >>= (return . Done))


instance MonadTrans ResT where
  lift m = Pause (m >>= (\x::a -> return (return x)))


instance MonadWriter obs m => MonadWriter obs (ResT m) where
  tell = lift . tell
  listen m = lift (listen (runResT m))
  pass m = lift (pass (runResT m))

\end{code}
