(\V x -> \V y -> 
    callcc (x / if y == 0 then k 0 else y)) 24 3
