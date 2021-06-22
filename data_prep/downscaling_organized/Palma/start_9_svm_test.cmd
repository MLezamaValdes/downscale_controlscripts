#!/bin/bash

# set the number of nodes
#SBATCH --nodes=3

# set the number of CPU cores per node
#SBATCH --ntasks-per-node 5

# How much memory is needed (per node)
#SBATCH --mem=10GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=2:00:00

# set name of job
#SBATCH --job-name=ffs_svmLinear_SE_F_test

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output output_9_training_ffs_svmLinear_SE_F_test.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla 9_FFS_models_svmLinear_SE_F_test.R