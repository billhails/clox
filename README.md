# clox

This is a working through of Bob Nystrom's `clox` interpreter, a garbage-collecting stack-based VM written in C,
from his excellent book [Crafing Interpreters](http://craftinginterpreters.com/) which I highly recommend.

There's nothing much in here that isn't in the book, though I have added anonymous functions, a `typeof` native function,
`do`/`while`, `switch` and a new linked-list type with operators for `cons` (infix `@`, right-associative),
`car` (prefix `<`), `cdr` (prefix `>`) and `append` (infix `@@`, right-associative),
along with a syntax sugaring for proper lists i.e. `[1, 2, 3]` desugars to `1 @ 2 @ 3 @ nil`.

Quite fortuitously and neatly, those prefix `car` and `cdr` operators compose, so `caddr` is `<>>`, `cadar` is `<><` etc. :grinning:
