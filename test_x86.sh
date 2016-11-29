#!/bin/bash

perform_test()
{
	# Get the file we are testing on
	TEST_FILE_IN=$1.txt
	TEST_FILE_OUT=$1.s
	TEST_FILE_EXE=$1

	if [ -f $TEST_FILE_IN ];
	then
		#Generate the x86 code
		src/CodeGenerator_x86.native $TEST_FILE_IN $TEST_FILE_OUT

		if [ -f $TEST_FILE_OUT ];
		then
			# Assemble into an executable
			gcc $TEST_FILE_OUT -o $TEST_FILE_EXE

			if [ -f $TEST_FILE_EXE ];
			then
				$TEST_FILE_EXE
			fi
		fi
	fi


}

# Perform tests written specifically for x86
for i in {0..8}
do
	perform_test test_cases/CodeGeneration/$i
done

# Perform regression tests
for i in {0..9}
do
	perform_test test_cases/small_$i
done

perform_test test_cases/large_iterative
perform_test test_cases/large_recursive
perform_test test_cases/large_fib
perform_test test_cases/util

for i in {0..19}
do
	perform_test test_cases/small_eval$i
done

for i in {0..3}
do
	perform_test test_cases/struct_$i
done

perform_test test_cases/linked_list
