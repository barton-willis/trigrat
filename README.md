# Trigonometric Rationalization for Maxima

The Maxima function `trigrat` attempts to simplify expressions that involve trigonometric expressions. Specifically, it converts trigonometric expressions into exponential form, simplifies them algebraically, and returns a simplified expression. 

This is a from‑scratch revision of the Maxima function `trigrat`, originally written by D. Lazard in August 1988. Since then, the code has been modified by many contributors. This version follows essentially the same method as the original, but introduces a new metric based on the number of trigonometric operators that guides the simplification process. Using this metric, the function may return the input expression unchanged when it contains fewer trigonometric operators than the simplified result. 

Because of this simplification heuristic, the function `trigrat` does *not* return a canonical representation — thus it is possible that `trigrat` will simplify equivalent expressions to syntactically distinct expressions. I will 
experiment with giving `trigrat` an optional argument that turns off the simplification heuristic.

This revision attempts to fix all open reported bugs in `trigrat`:

- [#4554](https://sourceforge.net/p/maxima/bugs/4554/) — `trigrat` often makes expressions unnecessarily complicated  
- [#2918](https://sourceforge.net/p/maxima/bugs/2918/) — `trigrat` crashes because it pollutes `?varlist`  
- [#2263](https://sourceforge.net/p/maxima/bugs/2263/) — `trigrat(sin(%pi/5))` produces an incorrect expansion

In fairness to the original author, it is possible that some of these bugs are due to changes to Maxima or to bugs
introduced to the package after the initial version of `trigrat`. 

The experimental function `xtrigrat` is similar to `trigrat`, but it attempts to change the input as little as possible. For example, `xtrigrat` avoids expanding terms that are free of trigonometric operators, whereas `trigrat` will expand them.

### Examples

```maxima
(%i1) trigrat(sin(x)^2 + cos(x)^2);
(%o1)                                  1
(%i2) trigrat(sin(3*x)/(sin(x + %pi/3)));
(%o2)                   sqrt(3) sin(2 x) + cos(2 x) - 1
```
The function `trigrat` expands terms in the expression that do not depend on trigonometric functions:
```maxima
(%i3) trigrat((1+x)^3 + sin(x)^2 + cos(x)^2);
                               3      2
(%o3)                         x  + 3 x  + 3 x + 2
```
But the function `xtrigrat` attempts to preserve the structure of the input as much as possible:
```maxima
(%i4) xtrigrat((1+x)^3 + sin(x)^2 + cos(x)^2);
                                        3
(%o4)                            (x + 1)  + 1
```

### To‑do

  - Document how to load `trigrat` into Maxima.

  - Write user documentation. 

  - Write a regression test for `trigrat`. 

  - Incorporate feedback from Maxima users and developers and improve the code.

  - Make sure that for quotients of trigonometric polynomials, `trigrat` returns
    a trigonometric polynomial when ever this is possible. The code relies on
    rational simplification with `algebraic` set to true to do this. Although
    this seems to work, I'm not sure that it handles all the cases that it should.
  
