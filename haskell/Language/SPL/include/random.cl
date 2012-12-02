#pragma OPENCL EXTENSION cl_khr_fp64 : enable

#include "mwc64x/cl/mwc64x.cl"

struct generator_t {
    mwc64x_state_t state;
};

struct generator_t initialize(ulong seed)
{
    struct generator_t new_generator;
    MWC64X_SeedStreams(&(new_generator.state), seed, 1 << 40);
    return new_generator;
}

double uniform(struct generator_t * generator)
{
    uint random = MWC64X_NextUint(&(generator->state));
    return random / (double) UINT_MAX;
}

double normal(struct generator_t * generator)
{
    // Box-Muller method
    double u = min(0.999999, max(0.000001, uniform(generator)));
    double v = min(0.999999, max(0.000001, uniform(generator)));
    return sqrt(-2 * log(u)) * cos(2 * M_PI * v);
}

struct generator_t split(struct generator_t * generator) 
{
    // Suggested by David Barrie Thomas (the author of MWC64X)
    struct generator_t new_generator;
    new_generator.state.x = MWC64X_NextUint(&(generator->state));
    new_generator.state.c = MWC64X_NextUint(&(generator->state));      
    return new_generator;
}

