void main ()
{
  int x, divisor;
  bool prime;
  x = 121;
  divisor = 2;
  prime = 1;
  while ((divisor < x) && prime) 
   if (!(x % divisor))
     prime = 0;
   else
     divisor = divisor + 1; 
  printf(prime);
}