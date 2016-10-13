#!/bin/bash
#Run all small tests
src/Parser.native test_cases/small0.txt $1
src/Parser.native test_cases/small1.txt $1
src/Parser.native test_cases/small2.txt $1
src/Parser.native test_cases/small3.txt $1
src/Parser.native test_cases/small4.txt $1
src/Parser.native test_cases/small5.txt $1
src/Parser.native test_cases/small6.txt $1
src/Parser.native test_cases/small7.txt $1
src/Parser.native test_cases/small8.txt $1
src/Parser.native test_cases/small9.txt $1

#Run all larger tests
src/Parser.native test_cases/large_recursive.txt $1
src/Parser.native test_cases/large_iterative.txt $1
