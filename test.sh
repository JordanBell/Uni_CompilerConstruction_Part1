#!/bin/bash

# Function for performing an evaluation test on a file. Note: The file must have a corresponding file in the "Evaluation Results" directory with the same name.
perform_eval_test()
{
	# Get the file we are testing on
	TEST_FILENAME=$1
	ARG_1=$2

	# Log the results to a temp file
	src/Parser.native test_cases/$TEST_FILENAME $ARG_1 > /tmp/eval_result_log.txt

	# Print the result and whether or not they match the expected output
	printf "$(</tmp/eval_result_log.txt)"
	cmp --silent test_cases/Evaluation\ Results/$TEST_FILENAME /tmp/eval_result_log.txt && printf " \t(Success)\n" || printf " \t(Mismatch)\n"
}

clear

# Parse tests
printf "\n\t*** Exercise 1 ***\n"
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
src/Parser.native test_cases/large_recursive.txt $1
src/Parser.native test_cases/large_iterative.txt $1

# Evaluation Tests
printf "\n\n\t*** Exercise 2 ***\n"
perform_eval_test small_eval0.txt $1
perform_eval_test small_eval1.txt $1
perform_eval_test small_eval2.txt $1
perform_eval_test small_eval3.txt $1
perform_eval_test small_eval4.txt $1
perform_eval_test small_eval5.txt $1
perform_eval_test small_eval6.txt $1
perform_eval_test small_eval7.txt $1
perform_eval_test small_eval8.txt $1
perform_eval_test small_eval9.txt $1
perform_eval_test large_fib.txt $1

# Evaluation Tests
printf "\n\n\t*** Exercise 3 ***\n"
perform_eval_test small_eval10.txt $1
perform_eval_test small_eval11.txt $1
perform_eval_test small_eval12.txt $1
perform_eval_test small_eval13.txt $1
perform_eval_test small_eval14.txt $1
perform_eval_test small_eval15.txt $1
perform_eval_test small_eval16.txt $1
perform_eval_test small_eval17.txt $1
perform_eval_test small_eval18.txt $1
perform_eval_test small_eval19.txt $1

# Evaluation Tests
printf "\n\n\t*** Exercise 4 ***\n"
src/Parser.native test_cases/optimisation/small0.txt $1 -oc; echo ""
src/Parser.native test_cases/optimisation/small1.txt $1 -oc; echo ""
src/Parser.native test_cases/optimisation/small2.txt $1 -oc; echo ""
src/Parser.native test_cases/optimisation/small3.txt $1 -oc; echo ""
src/Parser.native test_cases/optimisation/small4.txt $1 -oc; echo ""
src/Parser.native test_cases/optimisation/small5.txt $1 -oc; echo ""
src/Parser.native test_cases/optimisation/small6.txt $1 -oc; echo ""
src/Parser.native test_cases/optimisation/small7.txt $1 -oc; echo ""
src/Parser.native test_cases/optimisation/small8.txt $1 -oc; echo ""
src/Parser.native test_cases/optimisation/small9.txt $1 -oc; echo ""
