# guile-gcc-unit-parser
GCC dumpfile parser in Guile

Usage:

$ gcc -fsyntax-only -fdump-tree-all-graph=foo.lu a.c
$ guile -L . main.scm

