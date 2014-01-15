# Resumption Monad Makefile
#
# Author : Perry Alexander
# Date : Sat Sep  3 11:04:09 CDT 2005
#
# Variables:
#  SOURCES - list of source file base names
#  DOCSOURCE - name of the documentation root file
#  ILIBLOC - location of the InterpreterLib library directory
#  GHC - compiler command
#  GHCFLAGS - option flags for ghc
#  LHS2TEX - lhs2ex executable
#  LHS2TEXFLAGS - option flags for lhs2TeX
#  HADDOCK - haddock executable
#  HADDOCKFLAGS - haddock execution flags
#  PDFLATEX - pdflatex executable
#  BIBTEX - bibtex executable
#
# Targets:
#  all - build the entire library set with documentation
#  libs - build the base libraries only 
#  tidy - clean up non-pdf files
#  clean - clean up everything
#  doc - build the pdf file documenting the library
#
# Revisions:
#  Sun Sep  4 08:47:15 CDT 2005 - wpa -- added dependencies for doc creation
#  Sun Sep  4 08:47:36 CDT 2005 - wpa -- added haddock support 
#  Sun Sep  4 09:37:58 CDT 2005 - wpa -- restructured libs and doc targets
#    to operate on lists of targets.
#
# Problems:
#  - Documentation files always build due to the way the destination filename
#    is constructed.

SOURCES = Resumption
DOCSOURCE = Resumption
LIBDIR = /Users/alex/lib/Haskell/Monad

ILIBLOC = ..

GHC = /usr/local/bin/ghc
GHCFLAGS = --make -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction -i$(ILIBLOC)
LHS2TEX = /usr/local/bin/lhs2TeX
LHS2TEXFLAGS = --math
HADDOCK = /usr/local/bin/haddock
HADDOCKFLAGS = -h
PDFLATEX = /sw/bin/pdflatex
BIBTEX = /sw/bin/bibtex

#-- Nothing below here should need editing

FILENAME_docsource = $(DOCSOURCE).hs
FILENAME_doctex = $(DOCSOURCE).tex
FILENAME_docpdf = $(DOCSOURCE).pdf
FILENAME_docdir = ./doc
FILENAME_dochtml = $(FILENAME_docdir)/$(DOCSOURCE).html

FILENAME_sources = $(patsubst %,%.hs,$(SOURCES))
FILENAME_objects = $(patsubst %,%.o,$(SOURCES))

all:	libs doc

doc:	$(FILENAME_dochtml)

$(FILENAME_dochtml):	$(FILENAME_docsource) ;
	$(HADDOCK) $(HADDOCKFLAGS) -o $(FILENAME_docdir) $(FILENAME_docsource)

libs:	$(FILENAME_objects)

$(FILENAME_objects): 	$(FILENAME_sources) ;
	$(GHC) $(GHCFLAGS) $(patsubst %.o,%.hs,$@)

install:
	rm -fr $(LIBDIR)
	mkdir $(LIBDIR)
	cp -r $(FILENAME_docdir) $(LIBDIR)
	cp *.hi *.o *.hs $(LIBDIR)

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
	rm -f doc/*

debug:
	echo $(DOCSOURCE)
	echo $(FILENAME_docsource)
	echo $(FILENAME_docpdf)
	echo $(FILENAME_dochtml)
	echo $(FILENAME_doctex)
	echo $(FILENAME_sources)
	echo $(LIBDIR)
