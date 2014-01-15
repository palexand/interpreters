% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Interp] {Interpreter type class}

\begin{comment}
\begin{code}
module Interp where
import Fix
import MFix
\end{code}
\end{comment}

\begin{code}
class ( Monad m
      ) => Interp m s v 
 where 
   interp :: s -> m v

instance ( Monad m
         , AlgebraC f (m v)) 
         => Interp m (Fix f) v 
 where 
   interp = pCata

-- The next instance declaration would be very nice but it 
-- overlaps with the above one
-- So we need to specifically define the instance declaration
{-
instance (MAlgebraC m f v) => Interp m f v where
	interp = pMCata
-}
 
\end{code}


















