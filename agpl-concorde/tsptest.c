#include "export.h"

#include <stdio.h>

int main (int argc, char **argv)
{
  const int cities  = 4;
  int costs[] = {
    0,
    2, 0,
    1, 2, 0,
    2, 1, 2, 0};

  int sol [4];
  int res;

  //  In the Ada side...
  int solve_atsp_problem (int, int*, int, char**, int*);

  res = solve_atsp_problem (cities, costs, argc, argv, sol);

  printf ("Result from call is %d\n", res);
  printf ("Best tour is: ");

  int i;
  for (i = 0; i < cities; i ++)
    printf ("%d ", sol [i]);

  printf ("\n");

  return 0;
}
