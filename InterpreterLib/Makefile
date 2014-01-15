# InterpreterLib Makefile
#
# Author : Perry Alexander
# Date : Wed Aug 10 13:18:26 CDT 2005
#
# Variables:
#  SOURCES - list of source file base names
#  DOCSOURCE - name of the documentation root file
#  ILIBLOC - location of the InterpreterLib library directory
#  GHC - compiler command
#  GHCFLAGS - option flags for ghc
#  LHS2TEX - lhs2ex executable
#  LHS2TEXFLAGS - option flags for lhs2TeX
#  PDFLATEX - pdflatex executable
#  BIBTEX - bibtex executable
#
# Targets:
#  all - build the entire library set with documentation
#  libs - build the base libraries only 
#  termlibs - build the libraries for the Terms module
#  tidy - clean up non-pdf files
#  clean - clean up everything
#  doc - build the pdf file documenting the library
#
# Revisions:
#  Thu Aug 18 21:09:44 CDT 2005 -- wpa - Added capability to build
#    elements of the terms directory as well as the base directory

SOURCES = Functors Algebras Modules SubType
DOCSOURCE = InterpreterLib

ILIBLOC = ..

GHC = /usr/local/bin/ghc
GHCFLAGS = --make -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction -i$(ILIBLOC)
LHS2TEX = /usr/local/bin/lhs2TeX
LHS2TEXFLAGS = --math
PDFLATEX = /sw/bin/pdflatex
BIBTEX = /sw/bin/bibtex

#-- Nothing below here that should need editing

FILENAME_docsource = $(DOCSOURCE).lhs
FILENAME_doctex = $(DOCSOURCE).tex
FILENAME_docpdf = $(DOCSOURCE).pdf

FILENAME_sources = $(patsubst %,%.lhs,$(SOURCES))
FILENAME_termsources = $(wildcard Terms/*.lhs)

all:	libs termlibs doc

doc:	;
	$(LHS2TEX) $(LHS2TEXFLAGS) $(FILENAME_docsource) > $(FILENAME_doctex)
	$(PDFLATEX) $(FILENAME_doctex)
	$(BIBTEX) $(DOCSOURCE)
	$(PDFLATEX) $(FILENAME_doctex)
	$(PDFLATEX) $(FILENAME_doctex)

tidy:	;
	rm -f *.dvi
	rm -f *~
	rm -f Terms/*~
	rm -f *.log
	rm -f *.aux
	rm -f *.bbl
	rm -f *.blg

clean: 	tidy ;
	rm -f *.pdf
	rm -f *.o
	rm -f *.hi
	rm -f Terms/*.o
	rm -f Terms/*.hi
	rm -f InterpreterLib.tex

libs:	;
	$(GHC) $(GHCFLAGS) $(FILENAME_sources)

termlibs:	libs ;
	$(GHC) $(GHCFLAGS) $(FILENAME_termsources)

debug:
	echo $(DOCSOURCE)
	echo $(FILENAME_docpdf)
	echo $(FILENAME_doctex)
	echo $(FILENAME_sources)
	echo $(FILENAME_termsources)
	