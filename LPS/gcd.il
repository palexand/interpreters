INT x		
INT y	
READ x		
READ y		
PUSHA x		// if x == y then goto END
LOAD		
PUSHA y
LOAD
SUB
JMPZ 33  
PUSHA x		// if y > x then goto 25
LOAD
PUSHA y
LOAD
SUB
JMPLZ 25
PUSHA x		// x = x - y
PUSHA x
LOAD
PUSHA y
LOAD
SUB
STORE		
GOTO 5
PUSHA y		// y = y - x
PUSHA y
LOAD
PUSHA x
LOAD
SUB
STORE
GOTO 5
WRITE x

