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


    
### Speculation on fixing some bugs

The user documentation says:

**Function:** `trigrat(expr)`

*Gives a canonical simplified quasilinear form of a trigonometrical expression. `expr` is a rational fraction of several `sin`, `cos`, or `tan` whose arguments are linear forms in some variables (or kernels) and `%pi/n` (with integer `n`) and
integer coefficients. The result is a simplified fraction whose numerator and denominator are linear in `sin` and `cos`. Thus `trigrat` linearizes whenever it is possible.*

In this context, I am not sure what “quasilinear” means, but the condition that `trigrat(expr)` “linearizes whenever it is possible” is *not* true; for example, the current `trigrat` and my rewrite both fail for this case:

```maxima
(%i1)	trigrat(sin(5*x)/sin(x+%pi/5));

(%o1)	((sin((30*x+2*%pi)/5)-%i*cos((30*x+2*%pi)/5)-sin(4*x)+%i*cos(4*x))*sin((50*x+%pi)/5)+(-(%i*sin((30*x+2*%pi)/5))-cos((30*x+2*%pi)/5)+%i*sin(4*x)+cos(4*x))*cos((50*x+%pi)/5)+(%i*cos(%pi/5)-sin(%pi/5))*
sin((30*x+2*%pi)/5)+(%i*sin(%pi/5)+cos(%pi/5))*cos((30*x+2*%pi)/5)+(sin(%pi/5)-%i*cos(%pi/5))*sin(4*x)+(-(%i*sin(%pi/5))-cos(%pi/5))*cos(4*x))/(sin((30*x+2*%pi)/5)^2+
(-(2*%i*cos((30*x+2*%pi)/5))-2*sin(4*x)+2*%i*cos(4*x))*sin((30*x+2*%pi)/5)-cos((30*x+2*%pi)/5)^2+(2*%i*sin(4*x)+2*cos(4*x))*cos((30*x+2*%pi)/5)+sin(4*x)^2-2*%i*cos(4*x)*sin(4*x)-cos(4*x)^2)
```
The result is a quotient of Fourier sums, not a Fourier sum. 

I think that this bug is due to the fact that the method misses the identity `5 (x + %pi/5)  = 5 x + %pi`. Noticing 
this fact allows `sin(5*x)/sin(x+%pi/5)` to be expressed as a rational function of just `exp(%i*(x + %pi/5))`. 
Both the current `trigrat` function and this rewrite, achieve the linearization by through rational simplification (`ratsimp`) with the option variable `algebraic` set to true. This works for some cases, but not all. 

Here is a step-by-step method that shows how a corrected method might work. The hard part that I have not yet done is automatically finding the linear relations between the arguments of the trigonometric functions. Here I do an example by hand:

```maxima

(%i2)	xxx : exponentialize(xxx);
(xxx)	(%e^(5*%i*x)-%e^(-(5*%i*x)))/(%e^(%i*(x+%pi/5))-%e^(-(%i*(x+%pi/5))))

(%i3)	xxx : subst(g, exp(%i*(x + %pi/5)),xxx);
(xxx)	(%e^(5*%i*x)-%e^(-(5*%i*x)))/(g-1/g)
```
Now recognize that `exp(%i*5*x) = -g^5`:
```maxima
(%i4)	xxx : subst(-g^5,exp(%i*5*x),%);
(xxx)	(1/g^5-g^5)/(g-1/g)

(%i5)	partfrac(%,g);
(%o5)	-g^4-g^2-1/g^2-1/g^4-1

(%i6)	subst(g=exp(%i*(x+%pi/5)),%);
(%o6)	-%e^(4*%i*(x+%pi/5))-%e^(2*%i*(x+%pi/5))-%e^(-(2*%i*(x+%pi/5)))-%e^(-(4*%i*(x+%pi/5)))-1

(%i7)	demoivre(%);
(%o7)	-(2*cos(4*(x+%pi/5)))-2*cos(2*(x+%pi/5))-1

(%i8)	ratsimp(%);
(%o8)	-(2*cos((20*x+4*%pi)/5))-2*cos((10*x+2*%pi)/5)-1
```maxima


### To‑do

  - Document how to load `trigrat` into Maxima.

  - Fix the way the code identifies kernels. 

  - Write user documentation. 

  - Decide if the function `xtrigrat` is worthy of developing (and possibly give it a new name).

  
