% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Memory] {Memory interface}

\begin{code}

module Memory ( Memory 
              , setVal 
              , getVal
              , initMem
	      , memSize
	      , showMem
	      , showStack
              ) where
import Array
import Basics

-- Initial Memory Size
memSize :: Loc
memSize = 10

type Memory v 	= Array Loc v

setVal::Memory v -> Loc -> v -> Maybe (Memory v)
setVal m d v | inRange (bounds m) d = Just (m // [(d,v)])
             | otherwise            = Nothing

getVal :: Memory v -> Loc -> Maybe v
getVal m d | inRange (bounds m) d = Just (m!d)
           | otherwise            = Nothing
           
initMem :: v -> Memory v
initMem v = 
    array (0, memSize-1) 
	      [(i,v)| i <- [0..memSize-1] ]

\end{code}

|showStack m l| shows the contents of |m| until 
   the index |l|. It is useful when the memory 
   simulates a stack.

\begin{code}
showStack :: (Show v
	   ) => Memory v -> Loc -> String
showStack m ix = 
    showUntil m ix

showMem :: (Show v
	   ) => Memory v -> String
showMem m =
    showUntil m memSize

showUntil m ix =
    concat ["(" ++ show i ++ "->" ++ 
	     show (m!i) ++ ") " 
	   | i <- [0..ix-1]
	   ]

	     
\end{code}
