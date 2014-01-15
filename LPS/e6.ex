LetL fac = \L x -> if (x==0) then 1 else (print x ; x * (fac (x - 1))) 
	in (fac 5)