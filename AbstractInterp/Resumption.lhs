\begin{code}
module Resumption where
------------------------------------------------------------------

--------------- The resumption monad transformer
data (Monad m) => ResT m a = Done a | Pause (m (ResT m a))
                              
instance Monad m => Monad (ResT m) where
    return v       = Done v
    Done v >>= f   = f v
    Pause m >>= f  = Pause (m >>= \r -> return (r >>= f))

-- The reactive resumption monad transformer
type Dialog q r a = (q,r->a)
data Monad m => ReactT q r m a = D a 
                               | P (Dialog q r (m (ReactT q r m a)))
                                         
instance Monad m => Monad (ReactT q r m) where
    return v      = D v
    D v >>= f     = f v
    P (r,s) >>= f = P (r, \rsp -> (s rsp) >>= \m -> return (m >>= f))  
                                 ---      ^^^"bind" ^^^^^^ "unit" on monad m


-- The "snapshot" resumption monad transformer
data (Monad m) => ObsT obs m a = Dn a | Ps obs (m (ObsT obs m a))
                                         
instance Monad m => Monad (ObsT obs m) where
    return = Dn
    (Dn v) >>= f = f v
    Ps o m >>= f = Ps o (m >>= \r -> return (r >>= f))

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- The monad transformer of nondeterministic interactive computations

type FinSet a = [a]

data (Monad m) => NReactT q r m a = ND a 
                                  | NP (Dialog q r (m (FinSet (NReactT q r m a))))

instance Monad m => Monad (NReactT q r m) where
    return v       = ND v
    ND v >>= f     = f v
    NP (r,s) >>= f = NP (r, \rsp -> (s rsp) >>= \ms -> return (map (\ m -> m >>= f) ms)) 

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------

{-
`MonadConstructions.hs' is Copyright (c) William L Harrison 2004-2005, 
All rights reserved, and is distributed as
free software under the following license.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the following
conditions are met:

- Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials provided
with the distribution.

- Neither name of the copyright holders nor the names of its
contributors may be used to endorse or promote products derived
from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR THE
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
\end{code}