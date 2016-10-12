#!/bin/bash
#Run all small tests
echo "Running all small test 0-9"
src/Parser.native test_cases/small0.txt
src/Parser.native test_cases/small1.txt
src/Parser.native test_cases/small2.txt
src/Parser.native test_cases/small3.txt
src/Parser.native test_cases/small4.txt
src/Parser.native test_cases/small5.txt
src/Parser.native test_cases/small6.txt
src/Parser.native test_cases/small7.txt
src/Parser.native test_cases/small8.txt
src/Parser.native test_cases/small9.txt

#Run all larger tests
echo "Running all large tests"
src/Parser.native test_cases/large_recursive.txt
src/Parser.native test_cases/large_iterative.txt
