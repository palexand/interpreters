# InterpreterLib Terms Makefile
#
# Author : Perry Alexander
#  Date: Thu Aug 18 21:08:17 CDT 2005
#
# Variables:
#  SOURCES - list of source file base names
#  DOCSOURCE - name of the documentation root file
#  ILIBLOC - location of the InterpreterLib library directory
#  GHC - compiler command
#  GHCFLAGS - option flags for ghc
#
# Targets:
#  all - build the term library files
#  libs - build the libraries only 
#  tidy - clean up non-pdf files
#  clean - clean up everything
#
# Revisions:

SOURCES = ArithTerm FixTerm IOTerm IfTerm ImperativeTerm LambdaTerm LetTerm ProcTerm RALTerm RecordTerm StringTerm SumTerm UnitTerm VarTerm
DOCSOURCE = Terms

GHC = /usr/local/bin/ghc
ILIBLOC = /Users/alex/lib/Haskell
GHCFLAGS = --make  -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction -i$(ILIBLOC)

#-- Nothing below here that should need editing

FILENAME_sources = $(patsubst %,%.lhs,$(SOURCES))

all:	libs

tidy:	;
	rm -f *.dvi
	rm -f *~
	rm -f *.log
	rm -f *.aux
	rm -f *.bbl
	rm -f *.blg

clean: 	tidy ;
	rm -f *.pdf
	rm -f *.o
	rm -f *.hi
	rm -f Terms.tex

libs:	;
	$(GHC) $(GHCFLAGS) $(FILENAME_sources)

debug:
	echo $(FILENAME_sources)
	