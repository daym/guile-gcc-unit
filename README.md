# guile-gcc-unit-parser
GCC dumpfile parser in Guile

Usage:

$ gcc -fsyntax-only -fdump-tree-all-graph=foo.lu a.c
$ guile -L . main.scm

# Limitations

The GCC dumpfile doesn't contain the parameter names of function parameters.
This is pretty bad but for wrapper generation we need to specify extra metadata anyhow:
- What to do with data pointed to pointers. Free it? Copy it? What?
- What kind of strings are strings, if there are any? Zero-terminated? What?
