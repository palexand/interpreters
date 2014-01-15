Language Prototyping System	(Jose E. Labra Gayo, 2000)

Further information: http://lsi.uniovi.es/~labra/LPS/LPS.html


Introduction
------------

- This system is part of my research work and has no warranty

- It works with Haskell98 (but requires overlapping instances and
	multi-parameter type classes)
- It has been tested under Hugs98 and GHC 4.08
 

Some expressions are:

	(3+2)
	if (3 > 2) then 4 else 5
	LetN fac = \N x -> if (x==0) then 1 else (x * (fac (x - 1))) in (fac 5)

Acknowledgements
----------------
- L. Duponcheel wrote a first system which inspired this one
- E. Meijer suggested the use of first class polymorphism for monad transformers
- Mark P. Jones and B. Gaster provided helpful comments on the use of first-class 
polymorphism


Enjoy!!

Jose E. Labra Gayo
labra@lsi.uniovi.es
