#include <stdio.h>
#include <stdlib.h>

void print(int x)
{
  printf("%d\n", x);
  exit(0);
}

int main()
{
  int y = 260;
  int* x = &y;
  *x = 15;
  // Put my code here. Move the result into the variable y
  print(y);
  return 1;
}
