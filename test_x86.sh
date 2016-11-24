#!/bin/bash

perform_test()
{
	# Get the file we are testing on
	TEST_ID=$1
	BASH_ARG_1=$2

  #Generate the x86 code
  src/CodeGenerator_x86.native test_cases/Code\ Generation/$TEST_ID.txt test_cases/Code\ Generation/Generated\ Code/x86/4.x86frag test_cases/Code\ Generation/Generated\ Code/x86/2.x86frag $BASH_ARG_1

  #Sandwich the generated x86 code between the template prefix and suffix
  cd test_cases/Code\ Generation/Generated\ Code/x86
  cat *.x86frag > executable_$TEST_ID.s
  #cat executable.s
  gcc executable_$TEST_ID.s -o executable_$TEST_ID
  #rm executable.s
  ./executable_$TEST_ID
  cd ../../../../
}

for i in {0..8}
do
	perform_test $i
done
