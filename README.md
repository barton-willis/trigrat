# Trigonometric Rationalization for Maxima

This is a from scratch revision of the Maxima function `trigrat` that D. Lazard wrote in August 1988. Since then, the code has been modified and rewritten by many contributors. This version uses basically the same method as the orginal, but unlike the orignial, this code uses a metric based on number of trig operators to optionally return the expression unchanged.

The function `trigrat` does *not* return canonical representation--it is possible that trigrat will simplify equivalent expressions to syntactically distinct 
expressions. 

## Examples

```maxima
(%i1) trigrat(sin(x)^2 + cos(x)^2);
(%o1)                                  1
(%i2) trigrat(sin(3*x)/(sin(x + %pi/3)));
(%o2)                   sqrt(3) sin(2 x) + cos(2 x) - 1

