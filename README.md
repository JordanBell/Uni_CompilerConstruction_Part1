# Uni_CompilerConstruction_Part2
## Language Syntax

#### Let
"let" creates a constant identifier for a value, which may NOT be changed later.

Valid Usage:
`let x = 0 in
x`

Invalid Usage
`let x = 0 in
x = 15
x`

#### New
"new" creates a variable identifier for a value, which may be changed later. It must be dereferenced to access the value. Implicit dereferencing is not allowed.

Valid Usage:
`new x = 0 in
x = 2;
~x`

Invalid Usage:
`new x = 0 in
x = x + 1;
x`

#### Dereferencing
Use the `~` character in place of OCaml's `!` operator.
`int i = 0;`
`i = ~i + 1`

#### Referencing
Use the `&` character to create an identifier reference. This will reference another identifier, be it a constant, variable or function name.
`let x = 5 in
let y = &x in
x = 10
y // Now equals 10`

#### Function calls
Examples:
`foo(x)`
`bar(x, y, z)`

#### Comments
Comments are ignored by the lexer, and are marked by `//`, just as in C, Java, etc.

#### Notes
* Cannot have an if statement without a corresponding else expression
* Cannot initialise a variable without a succeeding expression sequence. For example, an expression with just `new i = 0` is invalid. This is because there is no expression for the `new` command to scope it within.
* Parenthesis are NOT recognised in the context of arithmetic or boolean logic. For example `(5 <= 6) || (5 >= 10)` is invalid.
* Scoping, ie wrapping an expression in curly braces, is superfluous, but I imagine will be used later. Helps keep things clean.
* It is a known issue that the grammar builds with reduce/reduce warnings.
* Function argument values are immutable. (ie `foo(x) { x = 1; ~x }` is invalid, as X is assumed to be a constant value. )
* Currently supports int, bool, string and struct types (where struct values are defined)
* Struct types must be create with new, instead of let (because accessing their members will essentially modify their value, making them non-const)

## How to Build
Entering `bash build.sh` will build all necessary files.

## How to Test
### Batch
Entering `bash test.sh` will run all tests and print out the results. `bash test.sh -verbose` will perform the tests and print out a parsed expression structure for the successful files.

### Individual files
All \*.txt test files are found within the `test_cases` directory. To run the program on a particular test file, for example test\_cases/small0.txt, enter the command: `src/Parser.native test\_cases/small0.txt`. Use the optional `-verbose` argument to the see the parsed expression structure.

## optimisation
* Activate optimisation with the -o option.
* The -oc option also evaluates with optimisations, but prints out optimisation statistics. If -verbose is set, this also prints out both optimised and non-optimised expression trees.
* Optimisation is applied 10 times in total

### Implemented optimisations
#### Constant folding
`1 + 2` folds into `Const_int 3`, and etc. for all integer and boolean operations (times, divide, geq, noteq, etc.)

#### Constant Propagation
`let x = 1 in let y = 2 in x + y` folds into `Const_int 3` as x and y are immutable constants.

#### State and Propagation
Propagation will also occur for new-bound variables up until they are first modified or overshadowed.

1. `new x = true in new y = false in ~x && !~y` becomes `Const_bool true` as x and y are not modified before being dereferenced. However, they are still declared and stored in case they are used later.
2.  `new x = 5 in new y = ~x in x = ~y * 2; ~y / ~x` WILL propagate when defining y, but will NOT propagate in the final evaluation, AFTER its modification. y will propagate fully as it is never modified. Both as still declared and stored in case they are used later.

#### Constant Inlining
When functions are supplied with constant arguments, either explicitly (ie `f(5)`), or after applying constant folding or propagation (`let x = 1 in let y = 2 in f(x + y)`), a function will be folded using those values and replaced inline after a further 10 optimisation iterations on that inline expression chunk.

#### Infinite Loop detection
When optimisation is enabled, the build will fail if it detects as infinite loop. This occurs when either its condition is evaluated as constantly true, or when condition contains variables that are never modified within the body of the while loop. This makes two assumptions which are as of writing always true: The program cannot break from within a loop (no "return", "continue" or "break" keywords exist), and no concurrency will modify the conditional variables.
