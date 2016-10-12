# Uni_CompilerConstruction_Part1
## Language Syntax
I have followed the hard syntax provided in your example: Approximately C-style.

Example: `main () {int x=1; x=x+1; return x}`

## How to Build
Entering `bash build.sh` will build all necessary files.

## How to Test
#### Batch 
Entering `bash test.sh` will run all tests and print out the results.

#### Individual files
All *.txt test files are found within the `test_cases` directory. To run the program on a particular test file, for example test\_cases/small0.txt, enter the command: `src/Parser.native test\_cases/small0.txt'
