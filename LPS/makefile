
# Standard defs:
OBJS = $(SRCS:.hs=.o)
GHC  = ghc
CC   = gcc
RM   = rm -f

BASICFLAGS = -fglasgow-exts \
	     -fallow-overlapping-instances \
	     -fallow-undecidable-instances

WARNINGS = 

PACKAGES = -package lang -package text

DEPENDFLAGS = $(PACKAGES)

FLAGS = $(BASICFLAGS)
FLAGS += $(WARNINGS)
FLAGS += $(PACKAGES)

# Specific

SRCS =	Basics.lhs Errors.lhs \
	Id.lhs Initial.lhs Runable.lhs \
        DebugMonad.lhs ShowMonad.lhs ErrMonad.lhs StateMonad.lhs \
	EnvMonad.lhs IOMonad.lhs ContMonad.lhs \
	MonadTs.lhs MonadT.lhs ErrorT.lhs EnvT.lhs StateT.lhs \
	DebugT.lhs IOT.lhs ContT.lhs \
	Fix.lhs MFix.lhs SubType.lhs SFunctor.lhs SemMonad.lhs \
	Shell.lhs Interp.lhs Lang.lhs Main.lhs \
	Error.lhs HasName.lhs HasParser.lhs \
	Heap.lhs Memory.lhs Table.lhs HashTable.lhs Stack.lhs \
	ParserUtils.lhs ShowUtils.lhs IOUtils.lhs \
	Lambda.lhs LParser.lhs LPretty.lhs LLang.lhs \
	While.lhs WParser.lhs WPretty.lhs WLang.lhs \
        ILProgram.lhs ILContext.lhs \

OBJS = 	Basics.o Errors.o \
	Id.o Initial.o Runable.o \
	DebugMonad.o ShowMonad.o ErrMonad.o StateMonad.o \
	EnvMonad.o IOMonad.o ContMonad.o \
	MonadTs.o MonadT.o ErrorT.o EnvT.o StateT.o \
	DebugT.o IOT.o ContT.o \
	Fix.o MFix.o SubType.o SFunctor.o SemMonad.o \
	Shell.o Interp.o Lang.o Main.o \
	Error.o HasName.o HasParser.o \
	Heap.o Memory.o Table.o HashTable.o Stack.o \
	ParserUtils.o ShowUtils.o IOUtils.o \
        Lambda.o LParser.o LPretty.o LLang.o \
	While.o WParser.o WPretty.o WLang.o \
        ILProgram.o ILContext.o

lps : $(OBJS)
	$(RM) $@
	$(GHC) -o $@ $(FLAGS) $(OBJS)

### Boilerplate GHC stuff

.SUFFIXES	: .lhs .hs .hi .o

depend :: 	$(SRC)
		$(GHC) -M $(DEPENDFLAGS) $(SRCS)

clean ::
	$(RM) *.o *.hi *.exe *~

%.hi		: %.o
		@:

%.o		: %.lhs
		$(RM) $@
		$(GHC) $(FLAGS) -c $< 

# DO NOT DELETE: Beginning of Haskell dependencies
Basics.o : Basics.lhs
Basics.o : ./Initial.hi
Errors.o : Errors.lhs
Errors.o : ./ErrMonad.hi
Id.o : Id.lhs
Initial.o : Initial.lhs
Runable.o : Runable.lhs
Runable.o : ./SemMonad.hi
Runable.o : ./MonadTs.hi
DebugMonad.o : DebugMonad.lhs
DebugMonad.o : ./IOMonad.hi
ShowMonad.o : ShowMonad.lhs
ShowMonad.o : ./MonadT.hi
ErrMonad.o : ErrMonad.lhs
StateMonad.o : StateMonad.lhs
EnvMonad.o : EnvMonad.lhs
IOMonad.o : IOMonad.lhs
ContMonad.o : ContMonad.lhs
MonadTs.o : MonadTs.lhs
MonadTs.o : ./MonadT.hi
MonadTs.o : ./ErrorT.hi
MonadTs.o : ./StateT.hi
MonadTs.o : ./EnvT.hi
MonadTs.o : ./IOT.hi
MonadTs.o : ./ContT.hi
MonadTs.o : ./DebugT.hi
MonadTs.o : ./Id.hi
MonadTs.o : ./SemMonad.hi
MonadT.o : MonadT.lhs
ErrorT.o : ErrorT.lhs
ErrorT.o : ./MonadT.hi
ErrorT.o : ./ShowMonad.hi
ErrorT.o : ./Id.hi
ErrorT.o : ./ErrMonad.hi
ErrorT.o : ./Error.hi
EnvT.o : EnvT.lhs
EnvT.o : ./MonadT.hi
EnvT.o : ./ShowMonad.hi
EnvT.o : ./Initial.hi
EnvT.o : ./EnvMonad.hi
StateT.o : StateT.lhs
StateT.o : ./MonadT.hi
StateT.o : ./StateMonad.hi
StateT.o : ./ShowMonad.hi
StateT.o : ./Initial.hi
DebugT.o : DebugT.lhs
DebugT.o : ./Initial.hi
DebugT.o : ./ShowMonad.hi
DebugT.o : ./Basics.hi
DebugT.o : ./ShowUtils.hi
DebugT.o : ./StateT.hi
DebugT.o : ./MonadT.hi
DebugT.o : ./IOMonad.hi
DebugT.o : ./DebugMonad.hi
IOT.o : IOT.lhs
IOT.o : ./Basics.hi
IOT.o : ./Initial.hi
IOT.o : ./MonadT.hi
IOT.o : ./ShowMonad.hi
IOT.o : ./ShowUtils.hi
IOT.o : ./StateT.hi
IOT.o : ./IOMonad.hi
ContT.o : ContT.lhs
ContT.o : ./Basics.hi
ContT.o : ./Initial.hi
ContT.o : ./MonadT.hi
ContT.o : ./ShowMonad.hi
ContT.o : ./ContMonad.hi
ContT.o : ./ShowUtils.hi
ContT.o : ./StateT.hi
Fix.o : Fix.lhs
MFix.o : MFix.lhs
MFix.o : ./Fix.hi
SubType.o : SubType.lhs
SFunctor.o : SFunctor.lhs
SFunctor.o : ./SubType.hi
SFunctor.o : ./HasParser.hi
SFunctor.o : ./Fix.hi
SFunctor.o : ./MFix.hi
SemMonad.o : SemMonad.lhs
SemMonad.o : ./HasName.hi
SemMonad.o : ./ErrMonad.hi
SemMonad.o : ./StateMonad.hi
SemMonad.o : ./EnvMonad.hi
SemMonad.o : ./IOMonad.hi
SemMonad.o : ./ContMonad.hi
SemMonad.o : ./DebugMonad.hi
SemMonad.o : ./Basics.hi
SemMonad.o : ./SubType.hi
SemMonad.o : ./Initial.hi
Shell.o : Shell.lhs
Shell.o : ./Error.hi
Shell.o : ./Lang.hi
Shell.o : ./Lambda.hi
Shell.o : ./While.hi
Shell.o : ./ILProgram.hi
Shell.o : ./ParserUtils.hi
Shell.o : ./HasName.hi
Shell.o : ./Initial.hi
Shell.o : ./IOUtils.hi
Interp.o : Interp.lhs
Interp.o : ./Fix.hi
Interp.o : ./MFix.hi
Lang.o : Lang.lhs
Lang.o : ./HasName.hi
Main.o : Main.lhs
Main.o : ./Lang.hi
Main.o : ./Shell.hi
Main.o : ./IOUtils.hi
Error.o : Error.lhs
HasName.o : HasName.lhs
HasName.o : ./SubType.hi
HasParser.o : HasParser.lhs
HasParser.o : ./Fix.hi
Heap.o : Heap.lhs
Heap.o : ./ErrMonad.hi
Heap.o : ./Initial.hi
Heap.o : ./Memory.hi
Heap.o : ./Basics.hi
Heap.o : ./MonadTs.hi
Heap.o : ./Runable.hi
Memory.o : Memory.lhs
Memory.o : ./Basics.hi
Table.o : Table.lhs
Table.o : ./ErrMonad.hi
Table.o : ./Initial.hi
Table.o : ./Basics.hi
Table.o : ./HashTable.hi
Table.o : ./Errors.hi
HashTable.o : HashTable.lhs
HashTable.o : ./Initial.hi
Stack.o : Stack.lhs
ParserUtils.o : ParserUtils.lhs
ShowUtils.o : ShowUtils.lhs
IOUtils.o : IOUtils.lhs
IOUtils.o : ./IOMonad.hi
Lambda.o : Lambda.lhs
Lambda.o : ./SemMonad.hi
Lambda.o : ./MonadTs.hi
Lambda.o : ./Fix.hi
Lambda.o : ./SFunctor.hi
Lambda.o : ./Interp.hi
Lambda.o : ./ShowUtils.hi
Lambda.o : ./IOUtils.hi
Lambda.o : ./Heap.hi
Lambda.o : ./Table.hi
Lambda.o : ./Errors.hi
Lambda.o : ./Lang.hi
Lambda.o : ./LLang.hi
Lambda.o : ./LPretty.hi
Lambda.o : ./LParser.hi
Lambda.o : ./Runable.hi
LParser.o : LParser.lhs
LParser.o : ./LLang.hi
LParser.o : ./Id.hi
LParser.o : ./Fix.hi
LParser.o : ./SFunctor.hi
LParser.o : ./SubType.hi
LParser.o : ./ParserUtils.hi
LPretty.o : LPretty.lhs
LPretty.o : ./LLang.hi
LPretty.o : ./Fix.hi
LPretty.o : ./ShowUtils.hi
LLang.o : LLang.lhs
LLang.o : ./Basics.hi
LLang.o : ./Fix.hi
LLang.o : ./SFunctor.hi
LLang.o : ./Initial.hi
While.o : While.lhs
While.o : ./WLang.hi
While.o : ./WParser.hi
While.o : ./WPretty.hi
While.o : ./MonadTs.hi
While.o : ./Runable.hi
While.o : ./Heap.hi
While.o : ./Table.hi
While.o : ./Errors.hi
While.o : ./IOUtils.hi
While.o : ./Lang.hi
While.o : ./HasName.hi
WParser.o : WParser.lhs
WParser.o : ./WLang.hi
WParser.o : ./ParserUtils.hi
WPretty.o : WPretty.lhs
WPretty.o : ./WLang.hi
WLang.o : WLang.lhs
ILProgram.o : ILProgram.lhs
ILProgram.o : ./ILContext.hi
ILProgram.o : ./HashTable.hi
ILProgram.o : ./Stack.hi
ILProgram.o : ./Memory.hi
ILProgram.o : ./IOUtils.hi
ILProgram.o : ./SemMonad.hi
ILProgram.o : ./Lang.hi
ILProgram.o : ./Runable.hi
ILContext.o : ILContext.lhs
ILContext.o : ./Stack.hi
ILContext.o : ./HashTable.hi
ILContext.o : ./Memory.hi
ILContext.o : ./Initial.hi
ILContext.o : ./MonadTs.hi
# DO NOT DELETE: End of Haskell dependencies
