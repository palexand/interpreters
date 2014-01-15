% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[IOUtils] {Input Output common utilities}

\begin{comment}
\begin{code}
module IOUtils( ask
	      , askStr
	      , askOption
	      , askOptionChar
	      , menuUntil
	      , menuUntil_
	      , doNothing
	      , menuSelect
	      , untilIO        -- untilIO :: (a -> Bool) -> (a -> IO a) -> a -> IO a
	      , untilIODebug   -- untilIO :: Show a => (a -> Bool) -> (a -> IO a) -> a -> IO a
	      , tryRead        -- :: Read a => String -> Maybe a
	      , getBetween
	      , shell
	      ) where
import List (intersperse)
import IOMonad
import Ix

\end{code}
\end{comment}

\begin{code}
ask::(Read a)=>String->IO a
ask msg = do
           putStrLn msg 
           line <- getLine
           case (reads line) of
		[(x,"")] -> return x
                _  -> do
                       putStr "Wrong input..."
                       ask msg

askStr::String->IO String
askStr msg = do
           putStrLn msg 
           line <- getLine
           return line


-- askOption msg options = shows 'msg' and reads the input from
--   the user until he types an option in opts
-- NOTE: I tried that it worked with all types in Read, but if you
--       try it with Char, it expects the char in quotes (like 'x') 
askOption::(Eq a, Read a, Show a)=>String->[a]->IO a
askOption msg opts = 
    do { putStrLn msg 
       ; line <- getLine
       ; case (reads line) of
      	[(x,"")] -> if x `elem` opts 
		    then return x
                    else do { putStrLn ("You must type: " ++ show opts ++ " " )
                            ; askOption msg opts
			    }
        _        -> do { putStr ("Wrong input...,  must be in " ++ show opts)
		       ; askOption msg opts
		       }
       }

-- askOptionChar msg options = shows 'msg' and reads the input from
--   the user until he types an option in opts
-- NOTE: This is askOption specialized for Char
askOptionChar::String->[Char]->IO Char
askOptionChar msg opts = 
    do { putStrLn msg 
       ; line <- getLine
       ; case line of
       	  (x:[]) -> if x `elem` opts 
	            then return x
                    else do { putStrLn ("You must type: " ++ opts ++ " " )
                            ; askOptionChar msg opts
			    }
          _  -> do { putStr ("Wrong input: " ++ line ++ " ... " )
                    ; askOptionChar msg opts
		    }
       }


menuUntil::(Eq a,Read a,Show a)=>
           String -> [(a, String, b -> IO b)] -> (a->Bool) -> b -> IO b
menuUntil title vals condFinal state = 
    do { putStrLn title
       ; newSt <- menuUntil' state
       ; return newSt
       }                           
 where menuUntil' st = 
	   do { opt <- ask msg
              ; newSt <- case lookup3 opt vals of
                  Just (_,action) -> 
		      do { newSt <- action st
                         ; return newSt
			 }
		  Nothing -> 
		      do { putStrLn "Bad Input..."
                         ; return st
			 }
               ; if condFinal opt 
		 then return newSt
                 else menuUntil' newSt
	       }
       (opts,msgs,actions) = unzip3 vals
       msg = concat ["\n"++ show o ++ " - " ++ m 
		    | (o,m) <- zip opts msgs
		    ]

menuUntil_::( Eq a
	    , Read a
	    , Show a
	    )=>
              String -> 
		  [(a, String, IO ())] -> 
		      (a->Bool) 
			  -> IO ()
menuUntil_ title vals condFinal = 
    do { putStrLn title
       ; menuUntil'
       }
 where menuUntil' = 
		do { opt <- ask msg
		   ; newSt <- case lookup3 opt vals of
                               Just (_,action) -> action 
                               Nothing         -> putStrLn "Bad Input..."
                   ; if condFinal opt then return ()    
                                      else menuUntil' 
		   }
       (opts,msgs,actions) = unzip3 vals
       msg = concat ["\n"++ (show o) ++ " - " ++ m | (o,m) <- zip opts msgs]


doNothing::( Eq a
	   , Show a
	   , Read a) => a -> IO a
doNothing st = return st

lookup3           :: Eq a => a -> [(a,b,c)] -> Maybe (b,c)
lookup3 k []       = Nothing
lookup3 k ((x,y,z):xys)
      | k==x      = Just (y,z)
      | otherwise = lookup3 k xys

menuSelect::String->    -- Menu Title
            [String]->  -- Options
            IO String
menuSelect title opts = 
    do { putStrLn title
       ; putStrLn (unlines [show n ++ " " ++ str
			   | (n,str) <- zip [1..] opts])
       ; n <- askOption "Option? " [1..length opts]
       ; return (opts!!(n-1))   
       }

untilIO :: (IOMonad io) => (a -> Bool) -> (a -> io a) -> a -> io a
untilIO end step x = 
    if end x then return x 
    else do { x' <- step x
	    ; untilIO end step x'
	    }

untilIODebug :: (IOMonad io
		, Show a
		) => (a -> Bool) -> (a -> io a) -> a -> io a
untilIODebug end step x = loop 1 x
    where 
     loop i x = if end x then return x 
		else do { x' <- step x
			; mPutStrLn ("Step " ++ show i ++ "\t" ++ show x')
			; loop (i+1) x'
			}

shell ::  (IOMonad io) =>
	    (a -> io b)            -- action
	      -> (b -> Bool)         -- end condition
	        -> (a -> b -> io a)   -- answer
		 -> a                   -- initial value
		  -> io b                   
shell action end answer x0 = 
	do { v <- action x0
      	   ; if end v then return v
	     else do { next <- answer x0 v
                     ; shell action end answer next
                     }
           }

tryRead :: Read a => String -> Maybe a                    
tryRead s = case readsPrec 0 s of
	     [(a,"")]  -> Just a
	     otherwise -> Nothing

getBetween :: ( Ix a
	      , Show a
	      , Read a
	      ) => (a, a) -> IO a
getBetween range = 
    do { xs <- getLine
       ; case tryRead xs of
	  Nothing -> do { putStrLn "Wrong value"
			; getBetween range
			}
			 
          Just v -> if inRange range v
		    then return v
		    else
                     do { putStrLn (
	                    "Value must be in range " ++ 
		            show range)
			; getBetween range
		        }
       }
	 

\end{code}


