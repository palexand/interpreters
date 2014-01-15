letV 
   add = \V x -> \V y -> x + 2*y
in 
  letV 
    f = \V x -> x + 1
  in
    letV
      update = \V f -> \V x -> x := f (get x)
    in
      get (new 0)

