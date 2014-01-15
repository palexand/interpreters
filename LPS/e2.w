program test is
var x : int
var fact : int
const c = 1
begin
 read x;
 fact := 1;
 while (x > 1) do
   fact := fact * x;
   x := x - 1
 endwhile;
 write fact
end
