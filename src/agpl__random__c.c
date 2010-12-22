#include <gsl/gsl_randist.h>
#include <stdio.h>
#include <time.h>

gsl_rng *r;

void agpl__random__gsl_init (void) {
    r = gsl_rng_alloc (gsl_rng_mt19937);
    gsl_rng_set (r, time(NULL));
}

void agpl__random__gsl_reset (void) {
    gsl_rng_set (r, time(NULL));
}

void agpl__random__gsl_reset_seed (int seed) {
    gsl_rng_set (r, (unsigned long int) seed);
}

double agpl__random__gsl_gaussian (double sigma) {
    return gsl_ran_gaussian (r, sigma);
}
