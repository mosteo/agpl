#include <limits.h>
#include <cstdio>
#include <cstring> // These are needed by some external 3rd party below

#include <agpl__boost.h>
#include <agpl__interfaces__c.h>

#include <boost/graph/adjacency_matrix.hpp>
#include <boost/graph/prim_minimum_spanning_tree.hpp>

using namespace boost;

extern "C" {

void agpl__boost__minimum_spanning_tree
  (double_array weights,       // assume a full graph... is a connect matrix
   int          num_vertices,
   double_array spanning_tree) // retains weight of used edges, -1 for non-used
{
    //  Weight as doubles
    typedef property<edge_weight_t, double> Weight;
    //  We use an adjacency matrix
    typedef adjacency_matrix<undirectedS,
                            no_property,
			    Weight> Graph;

    //  Edges are pairs of naturals:
    typedef std::pair < int, int > E;

    Graph g (num_vertices);

    //  Create graph with given weights
    for (int i = 0; i < num_vertices; i++)
       for (int j = 0; j < num_vertices; j++)
          if (i < j)
		if (weights [at (i, j, num_vertices)] < INF_WEIGHT()) {
		    add_edge
		      (i, j,
		       Weight (weights [at (i, j, num_vertices)]),
		       g);
		}

    //  And solve
    std::vector<graph_traits< Graph>::vertex_descriptor> pred (num_vertices);
    prim_minimum_spanning_tree (g, &pred[0]);

/*     for (int i = 0; i < num_vertices; i++) */
/*        printf ("TREE: %d -- %d\n", i, pred[i]); */

    for (int i = 0; i < num_vertices; i++)
	for (int j = 0; j < num_vertices; j++)
	    spanning_tree [at (i, j, num_vertices)] = INF_WEIGHT();

    for (int i = 0; i < num_vertices; i++) {
        spanning_tree [at (i, pred [i], num_vertices)] =
	      weights [at (i, pred [i], num_vertices)];
	spanning_tree [at (pred [i], i, num_vertices)] =
	      weights [at (pred [i], i, num_vertices)];
    }
}

void agpl__boost__test_prim (void)
{
    double_array w = {0, 5, 1, 0, 0, 2, 0, 0, 0};
    double       t[9];

    agpl__boost__minimum_spanning_tree (w, 3, t);
    for (int i = 0; i < 3; i++)
	for (int j = 0; j < 3; j++)
	    if (i < j && t [at (i, j, 3)] < INF_WEIGHT())
		printf("TREE: %d -- %d [W: %5.2f]\n", i, j, t [at (i, j, 3)]);
}

}
