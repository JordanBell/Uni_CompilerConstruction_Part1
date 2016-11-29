#!/bin/bash

perform_test()
{
	# Get the file we are testing on
	TEST_FILE_IN=$1.txt
	TEST_FILE_OUT=$1.s
	TEST_FILE_EXE=$1

  #Generate the x86 code
  src/CodeGenerator_x86.native $TEST_FILE_IN $TEST_FILE_OUT

	# Assemble into an executable
	gcc $TEST_FILE_OUT -o $TEST_FILE_EXE

	$TEST_FILE_EXE
}

# Perform tests written specifically for x86
for i in {0..8}
do
	perform_test test_cases/CodeGeneration/$i
done

# Perform regression tests
