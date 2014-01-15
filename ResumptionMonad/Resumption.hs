module Monad.Resumption where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans

-- * Basic resumption monad transformer defining pause and done

-- |Define ResT for representing resumption monads
data (Monad m) => ResT m a 
    -- |A completed process results in a value.
    = Done a
    -- |A paused process will behave like the resumption that is its argument.
    | Pause (m (ResT m a))

instance (Monad m) => Monad (ResT m) where
    return v = Done v
    (Done v) >>= f = f v
    (Pause m) >>= f = Pause (m >>= \r -> return (r >>= f))

-- |Methods specific to resumption monads
class (MonadResume m n) where
    -- |Step is essentially a lift function.
    step :: n a -> m n a

instance (Monad n) => MonadResume ResT n where
    step x = Pause (x >>= (return . return))

instance (MonadPlus m) => MonadPlus (ResT m) where
    mzero = Pause $ mzero
    (Pause x1) `mplus` (Pause x2) = Pause $ x1 `mplus` x2

instance MonadTrans ResT where
    lift = step

-- This is the old implementation of lift prior to realizing it is the
-- same function as step.
--    lift c = Pause $ do { c' <- c
--                        ; return $ return c'
--                        }

runResT :: (Monad m) => ResT m a -> m a
runResT (Done x) = return x
runResT (Pause x) = x >>= runResT

instance MonadError e m => MonadError e (ResT m) where
    throwError = lift . throwError
    catchError m f = lift (catchError (runResT m) (runResT . f))

instance MonadState s m => MonadState s (ResT m) where
    get = lift get
    put = lift . put

instance MonadReader e m => MonadReader e (ResT m) where
    ask = lift ask
    local f m = lift  (local f  (runResT m))

instance MonadWriter w m => MonadWriter w (ResT m) where
    pass = lift . pass . runResT
    listen = lift . listen . runResT
    tell = lift . tell

-- * Reactive resumption monad transformer defining query and response

-- |Define ReactT for representing reactive resumption monads.  req represents
-- a request being sent to an external data source.  rsp is the response to the
-- request and processed by a function that generates a monad representing
-- the reactive resumption monad's processing of the response.  Note that the
-- result of processing a response is another ReactT structure that could
-- request more data or represent a completed computation.
data ReactT req rsp m a
    -- |A completed process resulting in a value of type a
    = D a

    -- |A paused process sending query req and responding to query
    -- results by applying the response function.  req represents the
    -- query.  The function from rsp to a new monad represents the
    -- processing of the response.  The resulting monad encapsulates
    -- a ReactT that could result in a Paused or Completed process.
    | P req (rsp->m (ReactT req rsp m a))

-- Make ReactT a monad by defining return and bind.  Return is defined
-- directly while bind is defined by cases.
instance Monad m => Monad (ReactT req resp m) where
    -- return forms a terminating ReactT process with value v.  This
    -- represents the result of a process terminating with a value.
    return v = D v

    -- f :: a -> M b -- where M is the inner monad.  Represents a
    -- computation in the inner monad.
    
    -- Binding a completed ReactT process, (D v) over f simply calls f
    -- on the value associated with the completed process.  The result
    -- is of type M b.  Evaluating this monad will result in the value
    -- resulting from the inner computation.
    (D v) >>= f = f v

    -- bindM - bind inner monad
    -- bindR - bind resumption monad
    -- c - response from query q
    -- c -> (r c) ... - new f resulting in a inner monad.  c :: rsp

    -- Handler chooses a (P req r), processes req and responds with
    -- rsp.  The resulting computation is (r rsp) resulting in a new
    -- inner monad.  This result is then encapsulted in P:
    --
    --    (P req (r rsp))
    --
    -- Handler chooses this P again, processes req and rsponds with rsp'.
    -- The resulting process is:
    --
    --    (P req ((r rsp) rsp'))
    --
    -- and so forth.  This P represents the original P processing two
    -- message responses, rsp and rsp', in sequence:
    --
    --    (P req r)
    --    (P req (r rsp))
    --    (P req ((r rsp) rsp'))
    --    (P req (((r rsp) rsp') rsp''))
    --    ...
    --
    -- (r rsp) creates a new response function.  Remember that r and f
    -- are not the same function.
    -- 
    -- Assume rsp is the response to query q.  Applying :
    -- (r rsp) `bindM


    -- Given all this, binding a non-terminating process over f
    -- applies the response function to the response to generate a new
    -- inner monad.  
    (P q r) >>= f = P q (\c -> (r c) `bindM` (\k -> return (k `bindR` f)))
      where bindM m f = m >>= f
            bindR m f = m >>= f

    -- After receiving response from q:
    --   (r c') >>= (\k -> return (k >>= f))
    -- After evaluating (r c'):
    --   (M a) >>= (\k -> return (k >>= f))

    -- Assume that (M a) == (D v)
    --   (D v) >>= (\k -> return (k >>= f))
    -- Evaluating bind with (D v)
    --   (\k -> return (k >>= f))(D v)
    --   (return (v >>= f))
    --   (return (f v))
    --   (return v')
    --   (D v')

    -- Assume that (M a) == (P q' r')
    --   (P q' r') >>== \k -> (return (k >>= f))
    -- So, the new f is the lambda over k and evaluating the bind results in:
    --   (P q' (\c -> (r' c) >>= \j -> (return (j >>= \k -> return (k >>= f)))))
    --   (r' c') >>= \j -> (return j >>= (\k -> return (k >>= f)))
    -- Assume that (r' c') evalutes again to (D v)
    --   (D v) >>= \j -> return (j >>= (\k -> return (k >>= f)))
    --   (return (v >>= (\k -> (return (k >>= f)))
    --   (D (v >>= (\k -> (return (k >>= f)))))
    --   (D (return (v >>= f)))
    --   (D (D (f v)))
--                             (M a)
--    \bar{q}v | q(c).A.B
--  Equivalent to \c -> do { k <- (r c)
--                         ; return (k >>= f)  -- do { x <- k; f x }
--                         }

-- |Define a Signal class
class Signal a where
    ack :: a
    cont :: a

-- |Methods specific to reactive resumption monads
class MonadReact m req rsp | m -> rsp, m -> req where
    signal :: req -> m rsp

-- |Make ReactT a resumption monad
instance (Monad m, Signal a) => MonadResume (ReactT a a) m where
    step x = P cont (\_ -> x >>= (return . return))

-- |Make ReactT a reactive resumption monad
instance (Monad m) => MonadReact (ReactT req rsp m) req rsp where
    signal req = P req (return . return)

-- |Create a signal instance
instance Signal Int where
    ack = 0
    cont = 1
    
-- |Define plus and zero for ReactT
-- Currently unimplemented

-- |Define a lift function for ReactT.  Lift should evaluate it's
-- argument and lift the result into the ReactT monad.  Once again
-- it's the same as step and could be defined that way.  This makes
-- sense as step and lift are equivalent for ResT as well.
instance (Signal req) => MonadTrans (ReactT req rsp) where
    lift x = P cont (\_ -> x >>= (return . return))

{-
-- Can't define the automatic lifts due to having no standard
-- runReactT.  Must find a way to either define standard
-- schedule/handler pairs and write the transformers on a per
-- scheduler/handler basis.  The alternative is to define a collection
-- of ReactT monads that inherit from the basic ReactT.  Something
-- like ReactTLIFO, ReactTRR, etc.

instance MonadError e m => MonadError e (ResT m) where
    throwError = lift . throwError
    catchError m f = lift (catchError (runResT m) (runResT . f))

instance MonadState s m => MonadState s (ResT m) where
    get = lift get
    put = lift . put

instance MonadReader e m => MonadReader e (ResT m) where
    ask = lift ask
    local f m = lift  (local f  (runResT m))

instance MonadWriter w m => MonadWriter w (ResT m) where
    pass = lift . pass . runResT
    listen = lift . listen . runResT
    tell = lift . tell
-}
-- * Observation monad transformer defining observation points

-- |Define ObsT to represent snapshot resumption monads
data ObsT obs m a = Dn a | Ps obs (m (ObsT obs m a))

-- |Make ObsT a monad
instance (Monad m) => Monad (ObsT o m) where
    return = Dn
    (Dn v) >>= f = f v
    Ps o m >>= f = Ps o (m >>= \r -> return (r >>= f))

-- |Define methods specific to snapshot resumption monads
class MonadObserve obs m | m -> obs where
    observe :: obs -> m ()

-- |Make ObsT a snalshot resumption monad
instance (Monad m) => MonadObserve o (ObsT o m) where
    observe o = Ps o (return (return ()))
