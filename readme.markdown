# coatl

coatl is a work-in-progress dependently typed type-checker and
evaluator. One day it will grow up to be a compiler.

The only target executable here is `coatli`, an interactive
interpreter. It's usable now! There are three built-ins --
`Type`, the type of Types, `(->)`, a constructor of function
types, and `(~)`, which is used to represent the dependent
function space.

We can use `(~)` to represent `the` easily:

````coatl
the : Type ~ { a => a -> a };
the _ a = a;
````

Functions with multiple dependent parameters, though, become
unwieldy:

````coatl
const : Type ~ { a => Type ~ { b => a -> b -> a } };
const _ _ a _ = a;
````

An improvement to this situation is planned.

## Reading List

* [_A Tutorial Implementation of a Dependently-Typed
  Calculus_][LambdaPi] by Andres Löh, Conor McBride and Wouter Swierstra
* [_Eliminating Dependent Pattern-Matching_][edpm], by Healfdene Goguen,
  Conor McBride, and James McKinna
* [_Idris, a General Purpose Dependently-Typed Programming
  Language: Design and Implementation_][idris-impl], by the
  esteemed Edwin Brady.
* [_Towards a practical programming language based on dependent
  type theory_][agda-impl], by Ulf Norell

[LambdaPi]: http://www.andres-loeh.de/LambdaPi/
[edpm]: http://strictlypositive.org/goguen.pdf
[idris-impl]: http://eb.host.cs.st-andrews.ac.uk/drafts/impldtp.pdf
[agda-impl]: http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf
