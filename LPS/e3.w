program test is
var x : int
procedure fact (x) is
 var n:int
 var f:int
 begin
  write x;
  n := x;
  f := 1;
  while (n > 1) do
       f := f * n;
       n:=n-1
  endwhile;
  write f
 end
begin
 read x;
 fact(x);
 fact(x-1)
end
