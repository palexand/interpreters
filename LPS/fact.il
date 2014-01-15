INT var        -- read var
READ var
INT i          -- int i = 1
PUSHA i
PUSHC 1
STORE
INT res          -- int r = 1
PUSHA res
PUSHC 1
STORE
LABEL loop
PUSHA i        -- while i < var
LOAD
PUSHA var
LOAD
SUB
JMPLZ 23
PUSHA res        -- r = r * i
PUSHA res
LOAD
PUSHA i
LOAD
MUL
STORE
PUSHA i        -- i = i + 1
PUSHA i
LOAD
PUSHC 1
ADD
STORE
GOTO loop        -- end while
LABEL end
WRITE res      -- write res


