\begin{code}

module Errors where
import ErrMonad 

errUnbound id = 
    err $ "Unbound Name: " ++ id

errCannotAssign id = 
    err $ "Cannot assign a value to " ++ id

errExpectedInt v = 
    err $ "Type Error: value = " ++
	  show v ++ ", expected of type int"

errExpectedBool v = 
    err $ "Type Error: value = " ++ 
	  show v ++ ", expected of type bool"

errProcedureNotFound id =
    err $ "Procedure " ++ id ++ " not found"

errWrongInput l =
    err "Wrong input"

errWrongNumberArgs ls args =
    err $ "Wrong number of args = " ++ 
           show l ++ ", expected " ++ show n
 where n = length args
       l = length ls  

\end{code}
