#!/bin/bash

perform_test()
{
	# Get the file we are testing on
	TEST_ID=$1
	ARG_1=$2

  #Generate the x86 code
  src/CodeGenerator_x86.native test_cases/Code\ Generation/$TEST_ID.txt test_cases/Code\ Generation/Generated\ Code/x86/2.x86test $ARG_1

  #Sandwich the generated x86 code between the template prefix and suffix
  cd test_cases/Code\ Generation/Generated\ Code/x86
  cat *.x86test > executable_$TEST_ID.s
  #cat executable.s
  gcc executable_$TEST_ID.s -o executable_$TEST_ID
  #rm executable.s
  ./executable_$TEST_ID
  cd ../../../../
}

perform_test 0
perform_test 1
perform_test 2
perform_test 3
