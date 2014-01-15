\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Reactive where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State

import Resumption

--- Reactive resumption monad defining query and response
data ReactT req rsp m a
    = D a | P req (rsp->m (ReactT req rsp m a))

isDone (D _) = True
isDone  _ = False

instance Monad m => Monad (ReactT req resp m) where
    return v = D v
    (D v) >>= f = f v
    (P q r) >>= f = P q (\c -> (r c) >>= (\k -> return (k >>= f)))


instance (Signal req) => MonadTrans (ReactT req rsp) where
   lift m  = P cont (const (m >>=  (return . return)))

-- runReactT (P q r)
--                             (M a)
--    \bar{q}v | q(c).A.B
--  Equivalent to \c -> do { k <- (r c)
--                         ; return (k >>= f)  -- do { x <- k; f x }
--                         }

class Signal a where
    cont :: a

class MonadReact n req rsp | n -> req, n -> rsp where
    signal :: req -> n rsp

instance Monad m => MonadReact (ReactT req resp m) req resp where
    signal q = P q (return . return)

instance (Monad m, Signal req) => MonadResume (ReactT req rsp) m where
    step x = P cont (\_ -> x >>= (return . return))


instance (Signal req, MonadWriter w m) => MonadWriter w (ReactT req rsp m) where
  tell = lift . tell

instance (Signal req, MonadState s m) => MonadState s (ReactT req rsp m) where
  get = lift get
  put = lift . put
\end{code}
