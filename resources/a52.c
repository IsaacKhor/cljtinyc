void main()
{
 int a, b, c, d, e, f, g, h, x;
 a = 1; b = 2; c = 3;
 d = 4; e = 5; f = 6;
 g = 7; h = 8;
 x = 9;
 x = ((a + b + c + d + e + f + g + h)*(a - b + c - d + e - f + g - h) % g)
      *(x*(a + b + c + d + e + f + g + h)
         *(a - b + c - d + e - f + g - h) / b)
         *x*(a + b*(a + b + c + d + e + f + g + h)
               + c + d + e + f + g + h);
 printf(x);
}