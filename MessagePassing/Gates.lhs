\begin{code}

module Gates where

import MP
import Bus

import Control.Monad.Writer

data LineValue = X | H | L deriving (Show,Eq)

data AppMessages = Update LineValue deriving (Show,Eq)

instance Ord AppMessages where
    compare _ _ = EQ

isUpdate (Update _) = True
isUpdate _ = False

type Messages = BusMessages AppMessages

type GatesMonad a = Thread Messages (Writer String) a

binaryGate gname nout bf xbus ybus = (f `listeningToAs` (xbus,nx)) `listeningToAs` (ybus,ny)
    where nx = xbus ++ "?" ++ gname
          ny = ybus ++ "?" ++ gname
          f = do busReady xbus nx
                 busReady ybus ny
                 binaryGate' gname nout bf (xbus,nx,X) (ybus,ny,X)

binaryGate' gname nout bf x@(xbus,nx,vx) y@(ybus,ny,vy) = xbin <||> ybin
    where xbin = (appFilter nx isUpdate) ---> \(Tell _ (AppMsg (Update v))) -> do tellLn $ gname ++ " recved x" ++ (show (v,vy)) ++ ": " ++ nout ++ " to " ++ (show (v `bf` vy))
                                                                                  appTell nout (Update (v `bf` vy))
                                                                                  busReady xbus nx
                                                                                  binaryGate' gname nout bf (xbus,nx,v) y
          ybin = (appFilter ny isUpdate) ---> \(Tell _ (AppMsg (Update v))) -> do tellLn $ gname ++ " recved y" ++ (show (vx,v)) ++ ": " ++ nout ++ " to " ++ (show (vx `bf` v))
                                                                                  appTell nout (Update (vx `bf` v))
                                                                                  busReady ybus ny
                                                                                  binaryGate' gname nout bf x (ybus,ny,v)

lvAnd H H = H
lvAnd X _ = X
lvAnd _ X = X
lvAnd _ _ = L

lvOr H _ = H
lvOr _ H = H
lvOr X _ = X
lvOr _ X = X
lvOr _ _ = L

andGate gname nout narg1 narg2 = binaryGate gname nout lvAnd narg1 narg2
orGate gname nout narg1 narg2 = binaryGate gname nout lvOr narg1 narg2

setLV sout v = appTell ('!':sout) (Update v)

catchLV sout = f `listeningToAs` (sout,lname)
    where f = do tellLn $ "catching: " ++ sout
                 busReady sout lname
                 (appFilter lname isUpdate) ---> \(Tell _ (AppMsg (Update v))) -> (tellLn $ sout ++ " = " ++ (show v)) >> f
          lname = "catch[" ++ sout ++ "]"

run :: [GatesMonad ()] -> IO ()
run threads = putStrLn $ snd $ runWriter $ runMP' threads

wire n = ("bus " ++ n, bus n)

probe n = ("probe " ++ n, catchLV n)

setwire n v = ("setwire " ++ n, setLV n v)


wires = ([wire "a",wire "b",wire "c",wire "z1",wire "z2"]::[GatesMonad ()])

inits a b c = ([setwire "a" a, setwire "b" b, setwire "c" c]::[GatesMonad ()])

gates = ([("and a b -> z1",andGate "and1" "!z1" "a" "b"),("or c z1 -> z2",orGate "or1" "!z2" "c" "z1")]::[GatesMonad ()])

\end{code}
