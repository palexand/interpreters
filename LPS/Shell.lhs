% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Shell] {Shell}

\begin{comment}
\begin{code}
module Shell where
import Error(Error, ifError, ok, raise)
import Lang
import Lambda
import While
import ILProgram

import Parsec
import ParsecLanguage
import qualified ParsecToken as P
import ParserUtils

import List(intersperse)

import HasName
import Initial
import IOUtils
import System
\end{code}
\end{comment}

\begin{code}

version = "Version 1.0.0" ++
	  "(Jose E. Labra, http://lsi.uniovi.es/~labra)"

type DebugLevel = Integer

data Action = Quit
            | Help
	    | Load String
            | Exec
	    | Version
	    | Change
	    | SETDebug DebugLevel
	    | HelpSet
	    | Shell String
	    | Unknown String
 deriving Show

checkQuit :: Action -> Bool
checkQuit Quit = True
checkQuit _    = False


askAction :: MainState -> IO Action
askAction s = do { putStr prompt
                 ; input <- getLine
		 ; case parse pAction input input of
		    Left err -> return $ Unknown $ show err
		    Right a  -> return a
		 }
\end{code}

Action parser

\begin{code}

actionLang = 
  P.makeTokenParser 
     emptyDef { reservedNames = ["debug"] 
	      }

pAction = pComm <|> pShell

pComm = do { try colon
	   ; pQuit <|> 
	     pLoad <|> 
	     pExec <|> 
	     pHelp <|> 
	     pSet <|>
	     pChange <|>
             pVersion <?> "Command"
	   }
pQuit = pOpt "quit" >> return Quit
pHelp = pOpt "help" >> return Help
pVersion = pOpt "version" >> return Version

pLoad = do { pOpt "load"
	   ; whiteSpace
	   ; name <- pFileName
	   ; return $ Load name
	   }

pExec = do { pOpt "exec"
	   ; return $ Exec
	   }

pSet = do { pOpt "set"
	  ; pDebug <|> (return HelpSet) 
	  }

pChange = do { pOpt "change"
	     ; return Change
	     }

pDebug = 
    do { whiteSpace
       ; reserved "debug" 
       ; whiteSpace
       ; l <- pDebugLevel
       ; return $ SETDebug l
       }

pShell =
    do { try (char '!')
       ; s <- many anyChar
       ; return $ Shell s 
       }

pDebugLevel =  natural

pOpt (c:cs) = try (do { char c 
		      ; (try (string cs >> return ())) 
			<|> return ()
		      }
		  )

whiteSpace = P.whiteSpace actionLang
reserved   = P.reserved actionLang
colon      = P.colon actionLang
natural    = P.natural actionLang
	
\end{code}

State of Main loop

\begin{code}

data MainState = 
    ST { debug   :: DebugLevel
       , langs   :: [Lang]
       , current :: Lang             
       }

lambda = MkLang initial pLambda
while  = MkLang initial pWhile
ilang  = MkLang initial pIL

s0 :: MainState
s0 = ST { debug = 1 
	, langs = [lambda, while, ilang]
	, current = lambda
	}

instance Show MainState where
 show st = 
     "Debug: " ++ show (debug st) ++ "\n" ++ 
     "Languages: " ++ shLangs (langs st) ++ "\n" ++
     "Current Language: " ++ name (current st) ++ "\n"

shLangs :: [Lang] -> String
shLangs = concat . 
	  intersperse ", " . 
	  map name

answer :: MainState -> Action -> IO MainState

answer s Help = 
    do { putStrLn helpMsg
       ; return s
       }

answer s HelpSet = 
    do { putStrLn helpSetMsg
       ; print s
       ; return s
       }

answer s (Load fname) =
    catch 
      ( do { xs <- readFile fname
	   ; (b,s') <- newSyntax (current s) fname xs
	   ; if b 
	     then 
               do { 
		   info (debug s) 
		     ("Program Loaded: " ++ show s')
		  ; return s { current = s' }
		  }
             else 
	      return s
	   }
      )
      (\err -> do{ putStrLn $ "Loading error -> " ++ 
                          show err
		 ; return s
		 })

answer s Exec = do { exec $ current s
		   ; return s
		   }

answer s (SETDebug n) 
    | n `elem` debugLevels = 
           do { putStrLn $ "Debug = " ++ show n
	      ; return s { debug = n }
	      }
    | otherwise = 
	   do { putStrLn $ "Incorrect debug level, must be " ++ 
		           show debugLevels
	      ; return s
	      }

answer s Change =
    do { printLangs (langs s)
       ; putStrLn "Type number of language"
       ; n <- getBetween (1, length $ langs s)
       ; return $ s { current = (langs s)!!(n-1) }
       }

answer s Version = 
    do { putStrLn version
       ; return s
       }

answer s (Shell c) =
    do { system c
       ; return s
       }

answer s (Unknown e) = 
    do { putStrLn $ e
       ; return s
       }

answer s _ = 
    do { putStrLn "answer: unknown action!"
       ; return s
       }

debugLevels :: [DebugLevel]
debugLevels = [0..2]

info :: DebugLevel -> String -> IO ()
info l msg | l == 0 = return ()
	   | l >= 1 = putStrLn msg
	   | otherwise = error "info: negative DebugLevel"
			 
\end{code}

Common messages

\begin{code}
title  = " Modular Interpreter - " ++ version ++ "\n" ++
	 " Type :h for help\n"

prompt = " => "

bye    = "Bye!"

helpMsg :: String
helpMsg = showPretty (explanation:commands)

showPretty = concat . intersperse "\n" 

helpSetMsg :: String
helpSetMsg = showPretty (explainSet:options)

	 
commands :: [String]
commands = [ " :quit \t\t\t Quit program"
	   , " :help \t\t\t List of Commands" 
	   , " :load <filename> \t Load a program"
	   , " :exec \t Execute current program"
	   , " :version \t\t Version information"
	   , " :set <options> \t Set command line options"
	   , " :set \t\t\t help on command line options"
	   , " :change \t change current language"
	   , " ! <command> \t shell escape"
	   ]

explanation = 
    "\n"++
    "LIST OF COMMANDS:  Any command may be abbreviated to :c where\n" ++
    "c is the first character in the full name.\n"

explainSet =
    "\n" ++ "Command Line Options \n"

options = [" \t debug <integer> \t Set debug level"]
 

printLangs xs =
    putStrLn $  
    concat $
    intersperse "\n" 
     [show n ++ " - " ++ name l 
     | (n,l) <- zip [1..] xs
     ]

\end{code}