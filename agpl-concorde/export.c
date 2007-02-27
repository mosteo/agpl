/* This file contains functions created by me to be exported to the Ada side */

#include "export.h"

#include <stdio.h>

/* Returns 0 if ok, the concorde solver error code otherwise */
/* ncount is the number of nodes */
/* costs is an array of the form LOWER_DIAG_COST:
     i.e. the lower diagonal half of the cost matrix, including
     the diagonal (which should contain... 0 or infinite, I don't know?)
   sol is an array of ncount integers which will contain the solution
*/
int solve_tsp_problem (int  ncount,
			int  *costs,
			int  argc,
			char **argv,
                        int  *sol)
{
  CCdatagroup dat;
  int i, j, k;

  CCutil_init_datagroup (&dat);

  CCutil_dat_setnorm (&dat, CC_MATRIXNORM);

  dat.adj      = CC_SAFE_MALLOC (ncount, int*);
  dat.adjspace = CC_SAFE_MALLOC (ncount * (ncount + 1) / 2, int);

  for (i = 0, j = 0; i < ncount; i++) {
    dat.adj[i] = dat.adjspace + j;
    j += (i + 1);
  }

  for (i = 0, k = 0; i < ncount; i++)
    for (j = 0; j <= i; j++) {
      dat.adj[i][j] = costs[k];
      k++;
    }

  int rval;
  rval = CC_concorde_solve (ncount, &dat, sol, argc, argv);

  return rval;
}
