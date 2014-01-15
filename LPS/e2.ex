letN 
 fac = \N x -> if x==0 then 1 else x * fac (x - 1) 
in fac 5
