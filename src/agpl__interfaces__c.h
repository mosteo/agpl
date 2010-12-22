#ifndef Agpl__Interfaces__C_H
#define Agpl__Interfaces__C_H

extern "C" {

  typedef int    int_array[];
  typedef double double_array[];

  typedef int agpl_bool;

  const agpl_bool agpl_false = 0;
  const agpl_bool agpl_true  = 1;

  typedef int agpl_return_code;

  const agpl_return_code agpl_return_ok  = 0;
  const agpl_return_code agpl_return_err = 1;

  int at (int row, int col, int width);
  // Returns the index in a 1-d array organized row-first
  // The array is width cols wide
  // ******* 0-based *******

}

#endif
