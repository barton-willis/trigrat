# `trigrat`: Trigonometric Rationalization for Maxima

`trigrat` is a Maxima function (implemented in Lisp) that performs **controlled trigonometric rationalization**. It converts trigonometric expressions into exponential form, simplifies them algebraically, and returns a rationalized expression *only when doing so does not significantly increase structural complexity*. This makes `trigrat` a safe, predictable alternative to aggressive full‑expansion routines.

## Features

- Recursively processes Maxima expressions.
- Maps cleanly over:
  - mbags
  - sets
  - relational expressions
- Converts trigonometric operators to exponentials using `exponentialize`.
- Applies:
  - `sratsimp`
  - `expand`
  - `demoivre`
- Uses a **complexity‑based heuristic** to avoid pathological blow‑ups.
- Returns the original expression when rationalization would make the result worse.

## Motivation

Full trigonometric rationalization can produce extremely large intermediate expressions. In practice, many expressions benefit from rationalization, but some expand catastrophically. `trigrat` aims to provide a **balanced** approach:

- Rationalize when it helps.
- Decline when it hurts.

This makes it suitable for symbolic workflows where predictability and expression size control matter.

## How It Works

Given an expression `e`, `trigrat`:

1. **Handles atomic expressions** immediately.
2. **Recursively maps** itself over mbags, sets, and relational forms.
3. For general expressions:
   - Computes `n = trig-count(e) + 5`.
   - Converts trig functions to exponentials with `$exponentialize`.
   - Simplifies algebraically with `sratsimp` under `$algebraic = t`.
   - Expands the result.
   - Applies `$demoivre`.
   - Simplifies again with `sratsimp`.

The result is stored in `ans`.

### The heuristic threshold

The constant `5` is a **tolerance offset** added to `trig-count(e)` to form a threshold. Its purpose is pragmatic:

- Many benign rationalizations introduce a few extra trig operators as an artifact of exponential substitution and simplification.
- The offset allows for this harmless noise.
- If `trig-count(ans)` exceeds `trig-count(e) + 5`, the function assumes the rationalization has made the expression materially more complicated and returns the original input.

This is a **heuristic**, not a mathematical invariant: it reflects empirical behavior of typical expressions.

## Example

```maxima
(%i1) trigrat(sin(x)^2 + cos(x)^2);
(%o1) 1
