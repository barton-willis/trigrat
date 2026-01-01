# Trigonometric Rationalization for Maxima

The Maxima function `trigrat` attempts to simplify expressions that involve trigonometric expressions. Specifically, it converts trigonometric expressions into exponential form, simplifies them algebraically, and returns a simplified expression. For a [trigonometric polynomial](https://en.wikipedia.org/wiki/Trigonometric_polynomial), `trigrat` produces its Fourier representation; here are two examples:
```maxima
(%i1) trigrat(2 + 4*sin(x) + 6*sin(x)^2);
(%o1)                     - 3 cos(2 x) + 4 sin(x) + 

(%i2) trigrat(2 + 4*sin(x) + 6*sin(x)^2 * cos(2*x));
                     3 cos(4 x) - 6 cos(2 x) - 8 sin(x) - 1
(%o2)              - ──────────────────────────────────────
                                       2
```
Arguably, the Fourier representation is not a “simplification,” (meaning make presentable for publication) but the Fourier representation is *canonical*; by this we mean that:

- Every two mathematically equivalent trigonometric polynomials have identical Fourier representations.
- Distinct trigonometric polynomials have distinct Fourier representations.

For a *quotient* of trigonometric polynomials, `trigrat` produces a Fourier representation when possible; for
example
```maxima
(%i1) trigrat(sin(3*x)/sin(x + %pi/3));
(%o1)                   sqrt(3) sin(2 x) + cos(2 x) - 1
```

For inputs that are not trigonometric polynomials or quotients of them, `trigrat` produces its output following the 
steps:

- converts trigonometric functions to exponential form (`exponentialize`)
- rationally simplifies (`ratsimp`)
- expands
- converts from exponential form to trigonometric form (`demoivre`)
- rationally simplifies (`ratsimp`)

Some of the rational simplification is done with the option variable `algebraic` set to true.

As a convenience feature, `trigrat` has an optional second argument (default true) that when `false` forces 
`trigrat` to return the input unchanged when the process results in an expression with a greater number of trigonometric
functions; examples

```maxima
(%i1) trigrat(cos(x)^5*sin(2*x),true);
                 sin(7 x) + 5 sin(5 x) + 9 sin(3 x) + 5 sin(x)
(%o1)            ─────────────────────────────────────────────
                                      32
(%i2) trigrat(cos(x)^5*sin(2*x),false);
                                  5
(%o2)                          cos (x) sin(2 x)
```

### History
This is a from‑scratch revision of the Maxima function `trigrat`, originally written by D. Lazard in August 1988. Since then, the code has been modified by many contributors. This version follows essentially the same method as the original,
but this version attempts to fix all open reported bugs in `trigrat`. These bugs are:

- [#4554](https://sourceforge.net/p/maxima/bugs/4554/) — `trigrat` often makes expressions unnecessarily complicated  
- [#2918](https://sourceforge.net/p/maxima/bugs/2918/) — `trigrat` crashes because it pollutes `?varlist`  
- [#2263](https://sourceforge.net/p/maxima/bugs/2263/) — `trigrat(sin(%pi/5))` produces an incorrect expansion

In fairness to the original author, it is possible that some of these bugs are due to changes to Maxima or to bugs
introduced to the package after the initial version of `trigrat`. 

Additionally, this version is, I hope, easier to fix, extend, and maintain. 

### Experimental function `xtrigrat`
The experimental function `xtrigrat` is similar to `trigrat`, but it attempts to change the input as little as possible. For example, `xtrigrat` avoids expanding terms that are free of trigonometric operators, whereas `trigrat` will expand them. Some examples:

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

  - Fix the way `trigrat` behaves for expressions involving the trigonometric functions other than cosine and sine. 
    
  - Write a regression test for `trigrat`. 

  - Incorporate feedback from Maxima users and developers and improve the code and the algorithm.

  - Decide if the function `xtrigrat` is worthy of developing (and possibly give it a new name).

  - Both my code and the current code fail to linearize some quotients of trigonometric polynomials. The last section gives a possible way to fix this.  
    
### Speculation on a partial fraction-based algorithm

The user documentation says:

**Function:** `trigrat(expr)`

*Gives a canonical simplified quasilinear form of a trigonometrical expression. `expr` is a rational fraction of several `sin`, `cos`, or `tan` whose arguments are linear forms in some variables (or kernels) and `%pi/n` (with integer `n`) and
integer coefficients. The result is a simplified fraction whose numerator and denominator are linear in `sin` and `cos`. Thus `trigrat` linearizes whenever it is possible.*

In this context, I am not sure what “quasilinear” means, but the condition that `trigrat(expr)` “linearizes whenever it is possible” is *not* true; for example, the current `trigrat` and my rewrite both fail miserably for this case:

```maxima
(%i1)	trigrat(sin(5*x)/sin(x+%pi/5));

(%o1)	((sin((30*x+2*%pi)/5)-%i*cos((30*x+2*%pi)/5)-sin(4*x)+%i*cos(4*x))*sin((50*x+%pi)/5)+(-(%i*sin((30*x+2*%pi)/5))-cos((30*x+2*%pi)/5)+%i*sin(4*x)+cos(4*x))*cos((50*x+%pi)/5)+(%i*cos(%pi/5)-sin(%pi/5))*
sin((30*x+2*%pi)/5)+(%i*sin(%pi/5)+cos(%pi/5))*cos((30*x+2*%pi)/5)+(sin(%pi/5)-%i*cos(%pi/5))*sin(4*x)+(-(%i*sin(%pi/5))-cos(%pi/5))*cos(4*x))/(sin((30*x+2*%pi)/5)^2+
(-(2*%i*cos((30*x+2*%pi)/5))-2*sin(4*x)+2*%i*cos(4*x))*sin((30*x+2*%pi)/5)-cos((30*x+2*%pi)/5)^2+(2*%i*sin(4*x)+2*cos(4*x))*cos((30*x+2*%pi)/5)+sin(4*x)^2-2*%i*cos(4*x)*sin(4*x)-cos(4*x)^2)
```
The result is a quotient of Fourier sums, not a Fourier sum.  Both the current `trigrat` function and this rewrite, achieve the linearization by through rational simplification (`ratsimp`) with the option variable `algebraic` set to true. This works for some cases, but
not all. 

Here is a step-by-step partial fraction-based method that shows how this expression can be linearized:

```maxima

(%i1)	radsubstflag : true$

(%i2)	xxx : sin(5*x)/sin(x + %pi/5);
(xxx)	sin(5*x)/sin(x+%pi/5)

(%i3)	xxx : exponentialize(xxx);
(xxx)	(%e^(5*%i*x)-%e^(-(5*%i*x)))/(%e^(%i*(x+%pi/5))-%e^(-(%i*(x+%pi/5))))

(%i4)	xxx : ratsubst(g,exp(%i*x),xxx);
(xxx)	(%e^((%i*%pi)/5)*g^10-%e^((%i*%pi)/5))/(%e^((2*%i*%pi)/5)*g^6-g^4)

(%i5)	xxx : partfrac(xxx,g);
(xxx)	-(%e^((4*%i*%pi)/5)*g^4)-%e^((2*%i*%pi)/5)*g^2+%e^((3*%i*%pi)/5)/g^2+%e^((%i*%pi)/5)/g^4-1

(%i6)	xxx : demoivre(xxx);
(xxx)	-((%i*sin((4*%pi)/5)+cos((4*%pi)/5))*g^4)-(%i*sin((2*%pi)/5)+cos((2*%pi)/5))*g^2+(%i*sin((3*%pi)/5)+cos((3*%pi)/5))/g^2+(%i*sin(%pi/5)+cos(%pi/5))/g^4-

(%i7)	xxx : subst(exp(%i*x), g,xxx);
(xxx)	-((%i*sin((4*%pi)/5)+cos((4*%pi)/5))*%e^(4*%i*x))-(%i*sin((2*%pi)/5)+cos((2*%pi)/5))*%e^(2*%i*x)+(%i*sin((3*%pi)/5)+cos((3*%pi)/5))*%e^(-(2*%i*x))+(%i*sin(%pi/5)+cos(%pi/5))*%e^(-(4*%i*x))-

(%i8)	xxx : demoivre(xxx);
(xxx)	-((%i*sin((4*%pi)/5)+cos((4*%pi)/5))*(%i*sin(4*x)+cos(4*x)))+(%i*sin(%pi/5)+cos(%pi/5))*(cos(4*x)-%i*sin(4*x))-(%i*sin((2*%pi)/5)+cos((2*%pi)/5))*(%i*sin(2*x)+cos(2*x))+(%i*sin((3*%pi)/5)+cos((3*%pi)/5))*
(cos(2*x)-%i*sin(2*x))-

(%i9)	xxx : ratsimp(%);
(xxx)	(sin((4*%pi)/5)-%i*cos((4*%pi)/5)+sin(%pi/5)-%i*cos(%pi/5))*sin(4*x)+(-(%i*sin((4*%pi)/5))-cos((4*%pi)/5)+%i*sin(%pi/5)+cos(%pi/5))*cos(4*x)+(sin((3*%pi)/5)-%i*cos((3*%pi)/5)+sin((2*%pi)/5)-%i*cos((2*%pi)/5))*sin(2*x)+
(%i*sin((3*%pi)/5)+cos((3*%pi)/5)-%i*sin((2*%pi)/5)-cos((2*%pi)/5))*cos(2*x)-

(%i10)	load(ntrig)$

(%i11)	xxx : expand(xxx,0,0);
(xxx)	(-(((sqrt(5)+1)*%i)/4)-((-sqrt(5)-1)*%i)/4+((sqrt(5)-1)*sqrt(sqrt(5)+5))/2^(3/2))*sin(4*x)+((sqrt(5)+1)/4-(-sqrt(5)-1)/4)*cos(4*x)+(-(((sqrt(5)-1)*%i)/4)-((1-sqrt(5))*%i)/4+sqrt(sqrt(5)+5)/sqrt(2))*sin(2*x)+((1-sqrt(5))/4-(sqrt(5)-1)/4)*cos(2*x)-

(%i12)	ratsimp(%);
(%o12)	(sqrt(sqrt(5)+5)*((sqrt(2)*sqrt(5)-sqrt(2))*sin(4*x)+2^(3/2)*sin(2*x))+(2*sqrt(5)+2)*cos(4*x)+(2-2*sqrt(5))*cos(2*x)-4)/4
```