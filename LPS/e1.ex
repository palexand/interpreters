(new 0);
(0 := (read v));
(
 letV 
  fac = (\V x -> if (x==0) then 1 else x * fac (x - 1))
 in 
  (fac (get 0))
)

