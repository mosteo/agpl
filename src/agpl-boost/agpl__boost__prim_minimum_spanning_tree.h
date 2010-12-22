#include <agpl__interfaces__c.h>

extern "C" {

void agpl__boost__minimum_spanning_tree
  (double_array weights,
   int          num_vertices,
   double_array spanning_tree);
// weights: v x v adjacency matrix with weights (use INF_WEIGHT in agpl__boost for non-existing edges)
// spanning_tree: as weights, but non-belonging edges are set to INF_WEIGHT
// weights must be *fully informed*, not only half of it

}
