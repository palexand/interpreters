

This module tests the Heap implementation

\begin{code}
module TestHeap where
import Heap

type HeapSample = Heap (IO Int)


t' :: (ErrorT IO) (Loc, HeapSample)
t' = allocH (return 2) initial

t'' :: (ErrorT IO) Int
t'' = 
 do { -- Alloc a value with value 2
      (l,h) <- allocH (return 2) initial
    ; mv <- getHeap h l  -- obtain that value
    ; v <- mv
    ; mPutStrLn $ "1st value = " ++ show v
    ; h' <- setHeap h l (return (v + 2))
    ; mv' <- getHeap h' l
    ; v' <- mv'
    ; mPutStrLn $ "2nd value = " ++ show v'
    ; return v'
    }

t = run t''
\end{code}