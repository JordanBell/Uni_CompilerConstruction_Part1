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

## How to Build
Entering `bash build.sh` will build all necessary files.

## How to Test
### Batch
Entering `bash test.sh` will run all tests and print out the results. `bash test.sh -verbose` will perform the tests and print out a parsed expression structure for the successful files.

### Individual files
All \*.txt test files are found within the `test_cases` directory. To run the program on a particular test file, for example test\_cases/small0.txt, enter the command: `src/Parser.native test\_cases/small0.txt`. Use the optional `-verbose` argument to the see the parsed expression structure.
