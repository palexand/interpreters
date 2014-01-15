%
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[IL] {Instructions on the Intermediate Language}

\begin{comment}
\begin{code}
module HashTable ( HashTable
		 , addKey
		 , emptyTable
		 , updateKey
		 , deleteKey
		 , lookupKey
		 , values
		 ) where

import List (delete)
import Initial
\end{code}
\end{comment}

A naive Hash Table represented as a list of 
key-value pairs

\begin{code}

class Eq k => Key k

instance Key String

newtype Key k => HashTable k v = H [(k,v)] 
   {- It would be better to use a different 
      representation -}
    
addKey::Key key => key ->
	           value ->
	           HashTable key value -> 
		   HashTable key value
addKey k v (H t) = H ((k,v):t)

emptyTable :: Key key => 
	    HashTable key value
emptyTable = H []

values :: Key k => HashTable k v -> [(k,v)]
values (H t) = t

updateKey:: Key key => 
	    (value -> value) -> 
		value -> 
		    key -> 
			HashTable key value -> 
			    HashTable key value
updateKey f v0 k (H t) = H (updateLs f v0 k t)
 where updateLs f v0 k [] = [(k,v0)]
       updateLs f v0 k ((k1,v1):ts) = 
           if k == k1 then ((k,f v1):ts)
	     	      else (k1,v1):updateLs f v0 k ts

deleteKey::(Key key) => 
	   key -> 
	       HashTable key value -> 
		   HashTable key value
deleteKey k t = 
    maybe (error "DeleteKey: No element in Hash Table")
          (\_ -> H (deleteLs k (table2Ls t))) 
          (lookupKey k t)

lookupKey::Key key => 
	   key -> 
	       HashTable key value -> 
		   Maybe value
lookupKey k (H t) 
	  | null ls   = Nothing
          | otherwise = Just (head ls)
 where 
   ls = [val| (key,val) <- t, key == k] 

table2Ls (H t) = t

deleteLs k = 
    foldr (\(k',v) r -> 
	      if k == k' then r 
	      else (k',v):r) [] 


\end{code}

