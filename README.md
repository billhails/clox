# clox
My working through of Bob Nystrom's clox interpreter

This is  working through of Bob Nystrom's `clox` interpreter, a garbage-collecting stack-based VM written in C,
from his excellent book [Crafing Interpreters](http://craftinginterpreters.com/) which I highly recommend.

There's nothing much in here that isn't in the book, though I have added a `do`/`while` loop, a `switch` statement and
operators for `cons` (infix `@`), `car` (prefix `<`), `cdr` (prefix '>') and `append` (infix `@@`).
