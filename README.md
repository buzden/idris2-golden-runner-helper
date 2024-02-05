# A helper for custom runner of golden tests

Standard runner of [Idris2](https://github.com/idris-lang/Idris2)'s
[golden tests](https://github.com/idris-lang/Idris2/tree/main/libs/test) library
is very tunable and nice,
but is suited well for running from makefiles.

Package manager [pack](https://github.com/stefan-hoeck/idris2-pack/) supports running tests
put in special test packages, which is very convenient once you use `pack`.
However, standard runner from `Test.Golden` requires a lot of options to be passed at the command-line
which is not convenient if you want simply use `pack test <my-library>`.

So, this is an alternative runner of the same golden tests
which is friendly with simple use by `pack test` command.
It tries to be suitable both for running from interactive shell and CI with the same command.
