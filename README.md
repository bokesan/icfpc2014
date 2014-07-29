# A Storm of Minds ICFP Contest 2014 Entry

## Lambda-Man

### Tools

We implemented a compiler for a superficially Haskell-like language
to the GCC language.

- no anonymous functions, no partial application
- only simple let bindings
- no type checking

The last point proved most troublesome during development of the
lambda-man code.

Source code for the compiler is in directory `compiler/gcc`. The compiler is written
in Haskell. Use `cabal build` in directory compiler to build it.

### Strategy

Source code for our entry is in file `lambdaman.hs`. (We use the .hs suffix to get some
syntax highlighting in our usual editors). Compile with

    $ dist/build/gcc/gcc lambdaman.hs > lambdaman.gcc

The strategy for the lambdaman was to check a hierarchical set of heuristics to find a good move.
Top priorities were eating ghosts, eating fruits and getting power pills.
On the next bunch of heuristics, avoiding ghosts in general and getting near pills were checked.
The last things to kick in if nothing else provided a good path were survival rules, like not running into ghosts or dead ends.

To check possible paths, a recursive lookahead was used.
configuration parameters like depth of recursion, forced movement paradigm change in case of eventless times etc were chosen based on scoring, and proved to cause very wild and unpredictable chaotic behavior...


## Ghosts

### Tools

Ghosts are written in a slightly souped up version of the target
language. The assembler is written in Haskell and resides in
directory `compiler/ghasm`.

We don't use subroutines with return stack and calling conventions, but
just simple macro substitutions - except for relocations, we might
have got the same result with the C preprocessor.

### Strategy

We have just one ghost program, which slightly modifies its behavior
by taking the ghost index into account. Basically, ghosts move toward
the lambda unless frightened and reasonably near. They try to disperse
a bit when doing so, to increase the chance to actually surround the lambda.

We also have some form of dead-end detection, mostly to move out of
the starting hole on the classic map.

The ghost source code is in file `evil.gasm`. Compile with

    $ dist/build/ghasm/ghasm evil.gasm > ghost0.ghc

The final solution replaces the dynamic differences based on ghost index
by a preprocessor variant to generate 4 ghosts, for the off-chance that
that might slightly inconvenience lambda-men who actually analyze or simulate
the ghost code. There's a bash script that calls cpp to generate the 4 versions:

    $ ./make_ghosts.sh

