#pragma OPENCL EXTENSION cl_khr_fp64 : enable


// The local sum (of all thread values) is put in global_array[global_id].
// Requires: get_num_groups(0) < length(global_array)
// Requires: length(local_array) == get_local_size(0).
// Requires: get_local_size(0) is a power of two.
void local_sum(local double * local_array)
{
    int local_id = get_local_id(0);
    int local_size = get_local_size(0);

    barrier(CLK_LOCAL_MEM_FENCE);

    int span = 2;
    while(span <= local_size) {
        if(local_id % span == 0) {
            local_array[local_id] += local_array[local_id + span / 2];
        }
        span *= 2;
        barrier(CLK_LOCAL_MEM_FENCE);
    }
}


void emit_result(
    double value, 
    local double * local_means, 
    local double * local_standard_deviations, 
    global double * global_means,
    global double * global_standard_deviations
    )
{
    int local_id = get_local_id(0);
    int local_size = get_local_size(0);
    int group_id = get_group_id(0);

    local_means[local_id] = value;
    local_standard_deviations[local_id] = value;
    local_sum(local_means);
    
    double mean = local_means[0] / local_size;
    double distance = local_standard_deviations[local_id] - mean;
    local_standard_deviations[local_id] = distance * distance;
    local_sum(local_standard_deviations);
    
    if(local_id == 0) {
        global_means[group_id] = mean;
        double standard_deviation = (local_size > 1) ? sqrt(local_standard_deviations[0] / (local_size - 1)) : 0;
        global_standard_deviations[group_id] = standard_deviation;
    }
}


// Computes the combined mean and standard deviation into means[0] and standard_deviations[0] respectively.
// Requires: Must be run in a single work group (for memory synchronization).
// Requires: length(means) == length(standard_deviations) == global_count.
// Requires: global_count is a power of two.
// Requires: 2 * get_global_size(0) < global_count.
kernel void global_result(int local_size, global double * means, global double * standard_deviations)
{
    int id = get_global_id(0) * 2;
    int count = get_global_size(0) * 2;
    int span = 2;

    while(span <= count) {
        if(id % span == 0) {
            double standard_deviation1 = standard_deviations[id];
            double standard_deviation2 = standard_deviations[id + span / 2];
            double mean1 = means[id];
            double mean2 = means[id + span / 2];
            double mean = (mean1 + mean2) * 0.5;
            int n = local_size * (span / 2);
            double composed = 
                (n - 1) * (standard_deviation1 * standard_deviation1) +
                n * (mean1 * mean1) +
                (n - 1) * (standard_deviation2 * standard_deviation2) +
                n * (mean2 * mean2) -
                (n * 2) * (mean * mean);
            double standard_deviation = sqrt(composed / (n * 2 - 1));
            means[id] = mean;
            standard_deviations[id] = standard_deviation;
        }
        span *= 2;
        barrier(CLK_GLOBAL_MEM_FENCE);
    }
}

