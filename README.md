# clox

This is a working through of Bob Nystrom's `clox` interpreter, a garbage-collecting stack-based VM written in C,
from his excellent book [Crafing Interpreters](http://craftinginterpreters.com/) which I highly recommend.

There's nothing much in here that isn't in the book, though I have added a `do`/`while` loop, a `switch` statement and
operators for `cons` (infix `@`, right-associative), `car` (prefix `<`), `cdr` (prefix `>`) and `append` (infix `@@`, right-associative),
along with a list syntax sugaring i.e. `[1, 2, 3]` desugars to `(1 @ (2 @ (3 @ nil))).

Quite fortuitously and neatly, those prefix operators compose, so `caddr` is `<>>`, `cadar` is `<><` etc.
