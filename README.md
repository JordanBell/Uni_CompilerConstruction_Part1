# Uni_CompilerConstruction_Part1
## Language Syntax
The syntax is approximately C-style with a few exceptions
`main () {int x=1; x=x+1; return x}`	

#### Function arguments
Curly brackets can be used to scope a group of arguments. Both of the following are allowed.
`foo(x)`
`foo({x, y})`

#### Dereferencing 
Use the `~` character in place of OCaml's `!` operator.
`int i = 0;`
`i = ~i + 1`

#### Notes
* 'Let' is not supported (sorry), however 'New' is inferred from `int` type keyword.
* Cannot have an if statement without a corresponding else expression
* Cannot initialise a variable without a succeeding expression sequence. For example, an expression with just `int i = 0` is invalid. This is because there is no expression for the inferred `new` command to scope it within.
* Parenthesis are unrecognised in the context of arithmetic. For example `(5 <= 6) || (5 >= 10)` is invalid.
* Scoping, ie wrapping an expression in curly braces, is superfluous, but I imagine will be used later. Helps keep things clean.

## How to Build
Entering `bash build.sh` will build all necessary files.

## How to Test
### Batch 
Entering `bash test.sh` will run all tests and print out the results. `bash test.sh -verbose` will perform the rests and print out a parsed expression structure for the successful files.

### Individual files
All *.txt test files are found within the `test_cases` directory. To run the program on a particular test file, for example test\_cases/small0.txt, enter the command: `src/Parser.native test\_cases/small0.txt`. Use the optional `-verbose` argument to the see the parsed expression structure
