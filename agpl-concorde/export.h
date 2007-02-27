/* This file contains functions created by me to be exported to the Ada side */

#ifndef export_h
#define export_h

#include "concorde.h"

int CC_concorde_solve (int         ncount,
		       CCdatagroup *dat,
		       int         *sol,
		       int         ac,
		       char        **av);

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
                        int  *sol);

#endif
